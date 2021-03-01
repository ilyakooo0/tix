module Tix.Typechecker
  ( infer,
    solve,
    runFresh,
    runSeqWriter,
    getType,
    showNType,
    runNoTreeTracer,
  )
where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Internal (handleRelayS)
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.TreeTracer
import Control.Monad.Freer.Writer
import Data.Act
import qualified Data.Aeson as A
import Data.Fix
import Data.Foldable
import Data.Functor
import Data.Functor.Contravariant
import Data.Group hiding ((~~))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Shifted.Strict (ShiftedMap (..))
import qualified Data.Map.Shifted.Strict as SM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.MonoTraversable
import Data.Monoid
import qualified Data.RangeSet.Map as RS
import Data.Sequence (Seq (..))
import Data.Sequences
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Traversable
import Nix.Atoms
import Nix.Expr
import Nix.Pretty
import Tix.Types

getType :: NExprLoc -> (NType, [(SrcSpan, Errors)], [UnifyingError], A.Object)
getType r =
  let ((((((t, traceTree), unifying), errs), srcs), subs), _) =
        run
          . evalState (M.empty :: VariableMap)
          . runConstraintEnv
          . runState @[(TypeVariable, SrcSpan)] []
          . interpret @(Writer (TypeVariable, SrcSpan)) (\(Tell x) -> modify (x :))
          . runSeqWriter @[(SrcSpan, Errors)]
          . runSeqWriter @[UnifyingError]
          . runFresh
          . runTreeTracer
          $ inferGeneral r
   in (sub subs t, errs, unifying, traceTree)

runConstraintEnv ::
  Member (State VariableMap) effs =>
  Eff (SubstituteEnv : Writer Substitution : Writer Constraint : State (Seq Constraint) : effs) a ->
  Eff effs ((a, Substitution), Seq Constraint)
runConstraintEnv =
  runState @(Seq Constraint) Empty
    . interpret @(Writer Constraint) (\(Tell x) -> modify (x :<|))
    . runWriter @Substitution
    . interpret @SubstituteEnv
      ( \(SubstituteEnv x) -> do
          modify @(Seq Constraint) (subCon x <$>)
          modify @VariableMap (sub x <$>)
          tell x
          return ()
      )

data SubstituteEnv a where
  SubstituteEnv :: Substitution -> SubstituteEnv ()

subEnv :: Member SubstituteEnv r => Substitution -> Eff r ()
subEnv = send . SubstituteEnv

data Constraint = !NType :~ !NType
  deriving stock (Eq, Ord, Show)

instance Pretty Constraint where
  pretty (x :~ y) = pretty x <+> "~" <+> pretty y

newtype DeBrujinContext = DeBrujinContext (Sum Int)
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, Group, Num)

instance Act DeBrujinContext DeBrujin where
  act (DeBrujinContext (Sum n)) (DeBrujin i j) = DeBrujin (i + n) j

type TDeBrujinMap = ShiftedMap DeBrujinContext TypeVariable DeBrujin

close :: Predicate TypeVariable -> NType -> NType
close (Predicate f) =
  run
    . evalState @TDeBrujinMap mempty
    . runReader @Int 0
    . evalState @Int 0
    . close'
  where
    -- State is the sequential number, Reader is the binding context number
    close' :: NType -> Eff '[State Int, Reader Int, State TDeBrujinMap] NType
    close' x@(NAtomic _) = return x
    close' x@(NBrujin _) = return x
    close' (NTypeVariable tv) | f tv = NBrujin <$> newbrujin tv
    close' x@(NTypeVariable _) = return x
    close' (x :-> y) = bndCtx $ (:->) <$> close' x <*> close' y
    close' (List x) = bndCtx $ List <$> close' x
    close' (NAttrSet x) = bndCtx $ NAttrSet <$> traverse close' x

    bndCtx m = do
      modify @TDeBrujinMap (SM.shift 1)
      x <- local @Int (+ 1) m
      modify @TDeBrujinMap (SM.shift (-1))
      return x
    newbrujin t = do
      get @TDeBrujinMap <&> SM.lookup t >>= \case
        Nothing -> do
          i <- ask
          j <- get
          modify @Int (+ 1)
          let x = DeBrujin i j
          modify @TDeBrujinMap (SM.insert t x)
          return x
        Just x -> return x

class Free s where
  free :: s -> Set TypeVariable

instance Free NType where
  free (NTypeVariable v) = S.singleton v
  free (NAtomic _) = S.empty
  free (List x) = free x
  free (NAttrSet attrs) = foldMap free . M.elems $ attrs
  free (x :-> y) = free x <> free y
  free (NBrujin _) = S.empty

instance Free a => Free [a] where
  free = foldMap free

instance Free Substitution where
  free (Substitution s) = free . M.elems $ s

newtype Substitution = Substitution {unSubstitution :: Map TypeVariable NType}
  deriving newtype (Eq, Ord, Show)

prettySubstitution :: Substitution -> A.Value
prettySubstitution = A.toJSON . M.mapKeysMonotonic (TL.pack . show) . fmap renderPretty . unSubstitution

-- | @older <> newer@
instance Semigroup Substitution where
  (Substitution x) <> y'@(Substitution y) = Substitution (y <> fmap (sub y') x)

instance Monoid Substitution where
  mempty = Substitution mempty

sub :: Substitution -> NType -> NType
sub s r = case r of
  NAtomic _ -> r
  NBrujin _ -> r
  NTypeVariable x -> subTV x
  List x -> List $ sub s x
  NAttrSet m -> NAttrSet $ sub s <$> m
  x :-> y -> sub s x :-> sub s y
  where
    subTV x = M.findWithDefault r x $ unSubstitution s

subCon :: Substitution -> Constraint -> Constraint
subCon s (x :~ y) = sub s x :~ sub s y

(<<-) ::
  TypeVariable ->
  NType ->
  UnifyM Substitution
v <<- (NTypeVariable x) | v == x = return mempty
v <<- x | v `occursIn` x = tell (InfinityType x) >> return mempty
v <<- x = return $ Substitution $ M.singleton v x

unify :: Constraint -> UnifyM Substitution
unify = unifyWithPriority RS.empty

unifyWithPriority ::
  -- | The range of type variables to interpret as being lower
  -- priority (try to replace them if possible)
  RS.RSet TypeVariable ->
  Constraint ->
  UnifyM Substitution
unifyWithPriority range (lhs :~ rhs) =
  case (lhs, rhs) of
    (NAtomic x, NAtomic y) | x == y -> return mempty
    (NTypeVariable x, NTypeVariable y) ->
      if y `RS.member` range
        then x <<- NTypeVariable y
        else y <<- NTypeVariable x
    (NTypeVariable t, x) -> t <<- x
    (x, NTypeVariable t) -> t <<- x
    (List x, List y) -> unify $ x :~ y
    (NAttrSet x, NAttrSet y) -> do
      foldr (\c s -> s >>= \s' -> (<> s') <$> unify (subCon s' c)) (pure mempty)
        . M.elems
        $ M.intersectionWith (:~) x y
    (x1 :-> y1, x2 :-> y2) -> do
      s <- unify (x1 :~ x2)
      s' <- unify $ subCon s $ y1 :~ y2
      return $ s' <> s
    (x, y) -> tell (CanNotUnify x y) >> return mempty

solve :: Seq Constraint -> UnifyM Substitution
solve = solveWithPriority RS.empty

solveWithPriority ::
  -- | The range of type variables to interpret as being lower
  -- priority (try to replace them if possible)
  RS.RSet TypeVariable ->
  Seq Constraint ->
  UnifyM Substitution
solveWithPriority _ Empty = return mempty
solveWithPriority range (rest :|> c) = do
  s <- unifyWithPriority range c
  (<> s) <$> solveWithPriority range (fmap (subCon s) rest)

occursIn :: Free x => TypeVariable -> x -> Bool
occursIn t x = t `S.member` free x

data Errors
  = UndefinedVariable VarName
  | UnexpectedType {expected :: NType, got :: NType}
  | KeyNotPresent Text
  deriving stock (Eq, Ord, Show)

freshSrc ::
  Members '[Fresh, Reader SrcSpan, Writer (TypeVariable, SrcSpan)] r =>
  Eff r TypeVariable
freshSrc = do
  v <- send Fresh
  src <- ask @SrcSpan
  tell (v, src)
  return v

data UnifyingError
  = InfinityType NType
  | CanNotUnify NType NType
  deriving stock (Eq, Show)

type UnifyM x = forall r. Members '[Writer UnifyingError] r => Eff r x

type InferEffs =
  '[ Fresh,
     Writer (TypeVariable, SrcSpan),
     Writer Constraint,
     Writer (SrcSpan, Errors),
     State VariableMap,
     Writer UnifyingError,
     SubstituteEnv,
     TreeTracer
   ]

type InferM x = forall r. Members InferEffs r => Eff r x

type InferM' x = forall r. Members (Reader SrcSpan ': InferEffs) r => Eff r x

runSeqWriter :: forall s w r a. (IsSequence s, Element s ~ w) => Eff (Writer w ': r) a -> Eff r (a, s)
runSeqWriter = handleRelayS mempty (\s x -> return (x, s)) $ \s (Tell w) k -> k (cons w s) ()

throwSrc :: Members '[Reader SrcSpan, Writer (SrcSpan, e)] r => e -> Eff r ()
throwSrc e = do
  src <- ask @SrcSpan
  tell (src, e)

throwSrcTV ::
  Members
    '[ Reader SrcSpan,
       Writer (SrcSpan, e),
       Writer (TypeVariable, SrcSpan),
       Fresh
     ]
    r =>
  e ->
  Eff r TypeVariable
throwSrcTV e = do
  throwSrc e
  freshSrc

type VariableMap = Map VarName NType

(~~) :: Member (Writer Constraint) r => NType -> NType -> Eff r ()
lhs ~~ rhs = tell $ lhs :~ rhs

withBindings ::
  (Member (State VariableMap) effs) =>
  VariableMap ->
  Eff effs a ->
  Eff effs a
withBindings bindings m = do
  modify (bindings <>)
  a <- m
  modify @VariableMap (`M.difference` bindings)
  return a

infer :: NExprLoc -> InferM NType
infer (Fix (Compose (Ann src x))) = runReader src $ case x of
  NConstant a -> return . NAtomic $ case a of
    NURI {} -> URI
    NInt {} -> Integer
    NFloat {} -> Float
    NBool {} -> Bool
    NNull {} -> Null
  NLiteralPath {} -> return . NAtomic $ Path
  NEnvPath {} -> return . NAtomic $ Path
  NStr {} -> return $ NAtomic String
  NSym var ->
    get @VariableMap
      >>= maybe (NTypeVariable <$> throwSrcTV (UndefinedVariable var)) return . M.lookup var
  NList xs -> do
    xs' <- traverse infer xs
    traverse_ tell . fmap (uncurry (:~)) $ zip xs' (tail xs')
    case xs' of
      [] -> NTypeVariable <$> freshSrc
      y : _ -> return $ List y
  NUnary NNeg y -> do
    t <- infer y
    -- TODO: Infer either Float or Integer
    -- tell $ t :~ (normalType . NAtomic $ )
    return t
  NUnary NNot y -> do
    t <- infer y
    t ~~ NAtomic Bool
    return t
  NBinary NUpdate lhs rhs -> do
    lhsT <- infer lhs
    rhsT <- infer rhs
    lhsT ~~ rhsT
    lhs' <- expectAttrSet lhsT
    rhs' <- expectAttrSet rhsT
    forM_ (M.toList $ M.intersectionWith (,) lhs' rhs') (\(_, (a, b)) -> a ~~ b)
    return . NAttrSet $ rhs' <> lhs'
  NBinary NApp lhs rhs -> do
    lhst <- infer lhs
    rhst <- infer rhs
    rest <- NTypeVariable <$> freshSrc
    lhst ~~ (rhst :-> rest)
    return rhst
  NBinary NConcat lhs rhs -> do
    lhst <- infer lhs
    rhst <- infer rhs
    t <- NTypeVariable <$> freshSrc
    let listt = List t
    lhst ~~ listt
    rhst ~~ listt
    return $ listt
  NBinary op lhs rhs | op `elem` [NEq, NNEq] -> do
    lhst <- infer lhs
    rhst <- infer rhs
    lhst ~~ rhst
    return $ NAtomic Bool
  NBinary op lhs rhs
    | op `elem` [NLt, NLte, NGt, NGte] -> do
      lhst <- infer lhs
      rhst <- infer rhs
      -- TODO: Infer lhst and rhst are either Integer or Double
      lhst ~~ rhst
      return $ NAtomic Bool
  NBinary op lhs rhs
    | op `elem` [NPlus, NMinus, NMult, NDiv] -> do
      lhst <- infer lhs
      rhst <- infer rhs
      -- TODO: Infer lhst and rhst are either Integer or Double
      lhst ~~ rhst
      return $ lhst
  NBinary op lhs rhs | op `elem` [NAnd, NOr, NImpl] -> do
    lhst <- infer lhs
    rhst <- infer rhs
    rhst ~~ NAtomic Bool
    lhst ~~ NAtomic Bool
    return $ NAtomic Bool
  NSelect r path' def -> do
    setT <- infer r
    path <- getPath path'
    resT <- case path of
      Just p -> lookupAttrSet p setT
      Nothing -> NTypeVariable <$> freshSrc
    case def of
      Nothing -> return ()
      Just defR -> do
        defT <- infer defR
        defT ~~ resT
    return resT
  NSet NNonRecursive xs -> NAttrSet <$> inferBinding xs
  NHasAttr attrSet path -> do
    setT <- infer attrSet
    expectAttrSet setT
    return setT
  NAbs param body -> do
    (paramT, varMap :: VariableMap) <- case param of
      Param name -> do
        t <- freshSrc
        return (NTypeVariable t, M.singleton name $ NTypeVariable t)
      ParamSet set variadic binding -> do
        setBindings <-
          M.fromList <$> set
            `for` ( \(name, def) -> do
                      t <- case def of
                        Nothing -> NTypeVariable <$> freshSrc
                        Just def' -> infer def'
                      return (name, t)
                  )
        let setT = NAttrSet $ setBindings
        return (setT, maybe M.empty (`M.singleton` setT) binding <> setBindings)
    bodyT <- withBindings varMap $ infer body
    return $ paramT :-> bodyT
  NLet bindings body -> do
    bindingsT <- inferBinding bindings
    withBindings bindingsT $ infer body
  NIf cond t f -> do
    condT <- infer cond
    condT ~~ NAtomic Bool
    tT <- infer t
    fT <- infer f
    tT ~~ fT
    return tT
  NWith set body -> do
    setT <- infer set
    vars <- expectAttrSet setT
    withBindings vars $ infer body
  NAssert cond body -> do
    condT <- infer cond
    condT ~~ NAtomic Bool
    infer body
  NSynHole _ -> NTypeVariable <$> freshSrc

inferBinding :: [Binding (Fix NExprLocF)] -> InferM' (Map VarName NType)
inferBinding bindings = do
  bindings' <-
    bindings >>.= \case
      (NamedVar path r src) -> do
        getPath path >>= \case
          Nothing -> return []
          Just name -> do
            t <- inferGeneral r
            return $ [stack ((,) <$> name) (NAttrSet . uncurry M.singleton) t]
      Inherit attrSet keys src -> do
        vars <- case attrSet of
          Just attr -> do
            t <- infer attr
            expectAttrSet t
          Nothing -> get @VariableMap
        names <- catMaybes <$> traverse getKeyName keys
        return $
          catMaybes $
            names <&> \name -> case M.lookup name vars of
              Nothing -> Nothing
              Just r -> Just (name, r)
  return $ M.fromListWith mergeSetTypes bindings'

renderPretty :: Pretty x => x -> TL.Text
renderPretty = renderLazy . layoutPretty defaultLayoutOptions . pretty

showExpr :: Fix NExprLocF -> Text
showExpr = TL.toStrict . renderLazy . layoutPretty defaultLayoutOptions . P.group . prettyNix . stripAnnotation

inferGeneral :: NExprLoc -> InferM NType
inferGeneral x = traceSubtree (showExpr x) $ do
  (((t, subs), cs), range) <-
    traceSubtree "subexpressions" . registerTypeVariables . runConstraintEnv $ infer x
  traceSubtree "received" $ do
    traceValue "type" $ renderPretty t
    traceValue "substitutions" $ prettySubstitution subs
    traceValue "constraints" $ fmap renderPretty cs
  s' <- solveWithPriority range cs
  traceValue "substitutions_after_solving" $ prettySubstitution s'
  let s = subs <> s'
      returnedS = Substitution . M.filterWithKey (\k _ -> k `RS.notMember` range) . unSubstitution $ s
  traceSubtree "returned" $ do
    traceValue "substitutions" $ prettySubstitution returnedS
    -- Filter for optimization purposes
    subEnv returnedS
    let retT = close (Predicate (`RS.member` range) <> Predicate (`S.notMember` free returnedS)) $ sub s t
    traceValue "type" $ renderPretty retT
    return retT

expectAttrSet :: Members '[Reader SrcSpan, Writer (SrcSpan, Errors)] r => NType -> Eff r VariableMap
expectAttrSet (NAttrSet x) = return x
expectAttrSet g = do
  throwSrc
    UnexpectedType
      { expected = NAttrSet M.empty,
        got = g
      }
  return M.empty

-- | Stacks functions recursively inside each other
-- >>> stack [f1, f2, f3] h x
-- f1 (h $ f2 (h $ f3 x))
stack :: forall x y. NonEmpty (x -> y) -> (y -> x) -> x -> y
stack (g :| gs) h x = g $ f gs
  where
    f :: [x -> y] -> x
    f [] = x
    f (k : ks) = h . k $ f ks

(>>.=) :: (Applicative f, Monad t, Traversable t) => t a -> (a -> f (t b)) -> f (t b)
ta >>.= f = join <$> traverse f ta

mergeSetTypes :: NType -> NType -> NType
mergeSetTypes (NAttrSet ks) (NAttrSet ks') =
  NAttrSet $ M.unionWith mergeSetTypes ks ks'
mergeSetTypes _ x = x

-- TODO: process variables
antiAntiQuote :: Antiquoted (NString r) r -> Maybe Text
antiAntiQuote (Plain s) = unNString s
antiAntiQuote (EscapedNewline) = Just "\n"
antiAntiQuote (Antiquoted _) = Nothing

-- TODO: process variables
antiAntiQuoteText :: Antiquoted Text r -> Maybe Text
antiAntiQuoteText (Plain t) = Just t
antiAntiQuoteText EscapedNewline = Just "\n"
antiAntiQuoteText (Antiquoted _) = Nothing

-- TODO: process variables
unNString :: NString r -> Maybe Text
unNString (DoubleQuoted ss) = foldMap antiAntiQuoteText ss
unNString (Indented _ ss) = foldMap antiAntiQuoteText ss

getKeyName :: Monad m => NKeyName r -> m (Maybe VarName)
getKeyName (StaticKey k) = return $ Just k
getKeyName (DynamicKey k) = return $ antiAntiQuote k

getPath ::
  (Traversable t, Monad f) =>
  t (NKeyName r) ->
  f (Maybe (t Text))
getPath path = sequence <$> traverse getKeyName path

type AttrSetPath = NonEmpty Text

lookupAttrSet ::
  Members
    '[ Reader SrcSpan,
       Writer (SrcSpan, Errors),
       Writer (TypeVariable, SrcSpan),
       Fresh
     ]
    r =>
  AttrSetPath ->
  NType ->
  Eff r NType
lookupAttrSet path = go (NE.toList path)
  where
    go ::
      Members
        '[ Reader SrcSpan,
           Writer (SrcSpan, Errors),
           Writer (TypeVariable, SrcSpan),
           Fresh
         ]
        r =>
      [VarName] ->
      NType ->
      Eff r NType
    go [] x = return x
    go (p : ps) x = do
      expectAttrSet x <&> M.lookup p >>= \case
        Nothing -> NTypeVariable <$> throwSrcTV (KeyNotPresent p)
        Just t -> go ps t
