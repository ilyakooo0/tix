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

import Control.Exception (assert)
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
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Traversable
import Nix.Atoms
import Nix.Expr
import Nix.Pretty
import Tix.Types

getType :: NExprLoc -> (Scheme, [(SrcSpan, Errors)], [UnifyingError], A.Object)
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
          modify @(Seq Constraint) (sub x <$>)
          modify @VariableMap (sub x <$>)
          tell x
          return ()
      )

data SubstituteEnv a where
  SubstituteEnv :: Substitution -> SubstituteEnv ()

subEnv :: Member SubstituteEnv r => Substitution -> Eff r ()
subEnv = send . SubstituteEnv

data Constraint = !Scheme :~ !Scheme
  deriving stock (Eq, Ord, Show)

instance Pretty Constraint where
  pretty (x :~ y) = pretty x <+> "~" <+> pretty y

newtype DeBrujinContext = DeBrujinContext (Sum Int)
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, Group, Num)

instance Act DeBrujinContext DeBrujin where
  act (DeBrujinContext (Sum n)) (DeBrujin i j) = DeBrujin (i + n) j

type TDeBrujinMap = ShiftedMap DeBrujinContext TypeVariable DeBrujin

close :: Predicate TypeVariable -> Scheme -> Scheme
close (Predicate f) =
  run
    . evalState @TDeBrujinMap mempty
    . runReader @Int 0
    . evalState @Int 0
    . close'
  where
    -- State is the sequential number, Reader is the binding context number
    close' :: Scheme -> Eff '[State Int, Reader Int, State TDeBrujinMap] Scheme
    close' x@(NAtomic _) = return x
    close' (preds :=> t) =
      (:=>) <$> (closePred `traverse` preds) <*> case t of
        x@(NBrujin _) -> return x
        (NTypeVariable tv) | f tv -> NBrujin <$> newbrujin tv
        x@(NTypeVariable _) -> return x
        (x :-> y) -> bndCtx $ (:->) <$> close' x <*> close' y
        (List x) -> bndCtx $ List <$> close' x
        (NAttrSet x) -> bndCtx $ NAttrSet <$> traverse close' x

    closePred :: Pred -> Eff '[State Int, Reader Int, State TDeBrujinMap] Pred
    closePred = \case

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
  sub :: Substitution -> s -> s

instance Free NType where
  free (NTypeVariable v) = S.singleton v
  free (List x) = free x
  free (NAttrSet attrs) = foldMap free . M.elems $ attrs
  free (x :-> y) = free x <> free y
  free (NBrujin _) = S.empty

  sub s r = case r of
    NBrujin _ -> r
    NTypeVariable x -> NTypeVariable x -- it should be substituted higher in Scheme
    List x -> List $ sub s x
    NAttrSet m -> NAttrSet $ sub s <$> m
    x :-> y -> sub s x :-> sub s y

instance Free Pred

-- free (x `Update` y) = free x <> free y
-- free (x `HasField` (_, y)) = free x <> free y

-- sub s (x `Update` y) = sub s x `Update` sub s y
-- sub s (x `HasField` (t, y)) = sub s x `HasField` (t, sub s y)

instance Free Scheme where
  free (x :=> y) = free x <> free y
  free (NAtomic _) = S.empty
  sub s (x :=> NTypeVariable tv) =
    case M.lookup tv $ unSubstitution s of
      Nothing -> sub s x :=> NTypeVariable tv
      Just (newCs :=> new) -> (sub s x <> newCs) :=> new
      Just a@(NAtomic _) -> assert (null x) a
  sub s (x :=> y) = sub s x :=> sub s y
  sub _ x@(NAtomic _) = x

instance Free a => Free [a] where
  free = foldMap free
  sub s = fmap (sub s)

instance Free Substitution where
  free (Substitution s) = free . M.elems $ s
  sub new old = old <> new

instance (Free a, Free b) => Free (a, b) where
  free (a, b) = free a <> free b
  sub s (a, b) = (sub s a, sub s b)

newtype Substitution = Substitution {unSubstitution :: Map TypeVariable Scheme}
  deriving newtype (Eq, Ord, Show)

prettySubstitution :: Substitution -> A.Value
prettySubstitution = A.toJSON . M.mapKeysMonotonic (TL.pack . show) . fmap renderPretty . unSubstitution

-- | @older <> newer@
instance Semigroup Substitution where
  x'@(Substitution x) <> (Substitution y) = Substitution (fmap (sub x') y <> x)

instance Monoid Substitution where
  mempty = Substitution mempty

instance Free Constraint where
  free (x :~ y) = free x <> free y
  sub s (x :~ y) = sub s x :~ sub s y

(<<-) ::
  TypeVariable ->
  Scheme ->
  UnifyM Substitution
v <<- ([] :=> NTypeVariable x) | v == x = return mempty
v <<- x | v `occursIn` x = tell (InfinityType x) >> return mempty
v <<- x = return $ Substitution $ M.singleton v x

unifyWithPriority ::
  -- | The range of type variables to interpret as being lower
  -- priority (try to replace them if possible)
  RS.RSet TypeVariable ->
  Constraint ->
  UnifyM Substitution
unifyWithPriority range (lhs :~ rhs) =
  case (lhs, rhs) of
    (NAtomic x, NAtomic y) | x == y -> return mempty
    (xcs :=> NTypeVariable x, ycs :=> NTypeVariable y) -> do
      let ((_lhsCs, lhs'), (rhsCs, rhs')) =
            if y `RS.member` range
              then ((ycs, y), (xcs, x))
              else ((xcs, x), (ycs, y))
      lhs' <<- (rhsCs :=> NTypeVariable rhs') -- This might be wrong
    (_ :=> NTypeVariable t, x) -> t <<- x
    (x, _ :=> NTypeVariable t) -> t <<- x
    (xcs :=> List x, ycs :=> List y) -> do
      s <- unifyWithPriority range $ x :~ y
      s' <- uncurry unifyPred . sub s $ (xcs, ycs)
      return $ s <> s'
    (xcs :=> NAttrSet x, ycs :=> NAttrSet y) -> do
      s <-
        foldr (\c s -> s >>= \s' -> (<> s') <$> unifyWithPriority range (sub s' c)) (pure mempty)
          . M.elems
          $ M.intersectionWith (:~) x y
      s' <- uncurry unifyPred . sub s $ (xcs, ycs)
      return $ s <> s'
    (xcs :=> (x1 :-> y1), ycs :=> (x2 :-> y2)) -> do
      s <- unifyWithPriority range (x1 :~ x2)
      s' <- unifyWithPriority range $ sub s $ y1 :~ y2
      let ss = s' <> s
      s'' <- uncurry unifyPred . sub ss $ (xcs, ycs)
      return $ s'' <> ss
    (x, y) -> tell (CanNotUnify x y) >> return mempty

unifyPred :: [Pred] -> [Pred] -> UnifyM Substitution
unifyPred [] [] = return mempty
unifyPred _ _ = undefined

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
  (<> s) <$> solveWithPriority range (fmap (sub s) rest)

occursIn :: Free x => TypeVariable -> x -> Bool
occursIn t x = t `S.member` free x

data Errors
  = UndefinedVariable VarName
  | UnexpectedType {expected :: Scheme, got :: Scheme}
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
  = InfinityType Scheme
  | CanNotUnify Scheme Scheme
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

type VariableMap = Map VarName Scheme

(~~) :: Member (Writer Constraint) r => Scheme -> Scheme -> Eff r ()
lhs ~~ rhs = tell $ lhs :~ rhs

-- TODO: shadowed bindings are not properly restored.
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

instantiate ::
  forall effs.
  Members '[Fresh, Reader SrcSpan, Writer (TypeVariable, SrcSpan)] effs =>
  Scheme ->
  Eff effs Scheme
instantiate = evalState M.empty . instantiateScheme
  where
    instantiateScheme :: forall. Scheme -> Eff (State (Map DeBrujin TypeVariable) ': effs) Scheme
    instantiateScheme (ps :=> t) = (:=>) <$> traverse instantiatePred ps <*> instantiate' t
    instantiateScheme x@(NAtomic _) = return x

    instantiatePred :: forall. Pred -> Eff (State (Map DeBrujin TypeVariable) ': effs) Pred
    instantiatePred = \case
    -- instantiatePred (x `Update` y) = Update <$> instantiate' x <*> instantiate' y
    -- instantiatePred (x `HasField` (f, y)) = HasField <$> instantiate' x <*> ((f,) <$> instantiate' y)

    instantiate' :: forall. NType -> Eff (State (Map DeBrujin TypeVariable) ': effs) NType
    instantiate' x@(NTypeVariable _) = return x
    instantiate' (NBrujin db) = NTypeVariable <$> freshInst db
    instantiate' (x :-> y) = (:->) <$> instantiateScheme x <*> instantiateScheme y
    instantiate' (List x) = List <$> instantiateScheme x
    instantiate' (NAttrSet xs) = NAttrSet <$> traverse instantiateScheme xs

    freshInst :: DeBrujin -> Eff (State (Map DeBrujin TypeVariable) ': effs) TypeVariable
    freshInst db = do
      gets (M.lookup db) >>= \case
        Just t -> return t
        Nothing -> do
          t <- freshSrc
          modify (M.insert db t)
          return t

infer :: NExprLoc -> InferM Scheme
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
      >>= maybe (scheme . NTypeVariable <$> throwSrcTV (UndefinedVariable var)) return . M.lookup var
      >>= instantiate -- I have no idea if this covers all cases.
  NList xs -> do
    xs' <- traverse infer xs
    traverse_ tell . fmap (uncurry (:~)) $ zip xs' (tail xs')
    case xs' of
      [] -> scheme . NTypeVariable <$> freshSrc
      y : _ -> return . scheme $ List y
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
    return . scheme . NAttrSet $ rhs' <> lhs'
  NBinary NApp lhs rhs -> do
    lhst <- infer lhs
    rhst <- infer rhs
    rest <- scheme . NTypeVariable <$> freshSrc
    lhst ~~ (scheme $ rhst :-> rest)
    return rest
  NBinary NConcat lhs rhs -> do
    lhst <- infer lhs
    rhst <- infer rhs
    t <- scheme . NTypeVariable <$> freshSrc
    let listt = scheme $ List t
    lhst ~~ listt
    rhst ~~ listt
    return $ listt
  NBinary NEq lhs rhs -> equality lhs rhs
  NBinary NNEq lhs rhs -> equality lhs rhs
  NBinary NLt lhs rhs -> comparison lhs rhs
  NBinary NLte lhs rhs -> comparison lhs rhs
  NBinary NGt lhs rhs -> comparison lhs rhs
  NBinary NGte lhs rhs -> comparison lhs rhs
  NBinary NPlus lhs rhs -> math lhs rhs
  NBinary NMinus lhs rhs -> math lhs rhs
  NBinary NMult lhs rhs -> math lhs rhs
  NBinary NDiv lhs rhs -> math lhs rhs
  NBinary NAnd lhs rhs -> logic lhs rhs
  NBinary NOr lhs rhs -> logic lhs rhs
  NBinary NImpl lhs rhs -> logic lhs rhs
  NSelect r path' def -> do
    setT <- infer r
    path <- getPath path'
    resT <- case path of
      Just p -> lookupAttrSet p setT
      Nothing -> scheme . NTypeVariable <$> freshSrc
    case def of
      Nothing -> return ()
      Just defR -> do
        defT <- infer defR
        defT ~~ resT
    return resT
  NSet NNonRecursive xs -> scheme . NAttrSet <$> inferBinding xs
  NHasAttr attrSet path -> do
    setT <- infer attrSet
    expectAttrSet setT
    return setT
  NAbs param body -> do
    (paramT, varMap :: VariableMap) <- case param of
      Param name -> do
        t <- freshSrc
        return (scheme $ NTypeVariable t, M.singleton name . scheme $ NTypeVariable t)
      ParamSet set variadic binding -> do
        setBindings <-
          M.fromList <$> set
            `for` ( \(name, def) -> do
                      t <- case def of
                        Nothing -> scheme . NTypeVariable <$> freshSrc
                        Just def' -> infer def'
                      return (name, t)
                  )
        let setT = scheme . NAttrSet $ setBindings
        return (setT, maybe M.empty (`M.singleton` setT) binding <> setBindings)
    bodyT <- withBindings varMap $ infer body
    return . scheme $ paramT :-> bodyT
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
  NSynHole _ -> scheme . NTypeVariable <$> freshSrc
  where
    equality lhs rhs = do
      lhst <- infer lhs
      rhst <- infer rhs
      lhst ~~ rhst
      return $ NAtomic Bool
    comparison lhs rhs = do
      lhst <- infer lhs
      rhst <- infer rhs
      -- TODO: Infer lhst and rhst are either Integer or Double or String
      lhst ~~ rhst
      return $ NAtomic Bool
    math lhs rhs = do
      lhst <- infer lhs
      rhst <- infer rhs
      -- TODO: Infer lhst and rhst are either Integer or Double
      lhst ~~ rhst
      return lhst
    logic lhs rhs = do
      lhst <- infer lhs
      rhst <- infer rhs
      rhst ~~ NAtomic Bool
      lhst ~~ NAtomic Bool
      return $ NAtomic Bool

inferBinding :: [Binding (Fix NExprLocF)] -> InferM' (Map VarName Scheme)
inferBinding bindings = do
  bindings' <-
    bindings >>.= \case
      (NamedVar path r src) -> do
        getPath path >>= \case
          Nothing -> return []
          Just name -> do
            t <- inferGeneral r
            return $ [stack ((,) <$> name) (scheme . NAttrSet . uncurry M.singleton) t]
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

inferGeneral :: NExprLoc -> InferM Scheme
inferGeneral x = traceSubtree (showExpr x) $ do
  (((t, subs), cs), range) <-
    traceSubtree "subexpressions" . registerTypeVariables . runConstraintEnv $ infer x
  traceSubtree "received" $ do
    traceValue "type" $ renderPretty t
    traceValue "substitutions" $ prettySubstitution subs
    traceValue "constraints" $ fmap renderPretty cs
    traceValue "range" $ T.pack . show $ range
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

-- | Deprecated
expectAttrSet :: Members '[Reader SrcSpan, Writer (SrcSpan, Errors)] r => Scheme -> Eff r VariableMap
expectAttrSet (_ :=> NAttrSet x) = return x
expectAttrSet g = do
  throwSrc
    UnexpectedType
      { expected = scheme $ NAttrSet M.empty,
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

-- | Deprecated
mergeSetTypes :: Scheme -> Scheme -> Scheme
mergeSetTypes (xcs :=> NAttrSet ks) (ycs :=> NAttrSet ks') =
  (xcs <> ycs) :=> NAttrSet (M.unionWith mergeSetTypes ks ks')
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

-- | Deprecated
lookupAttrSet ::
  Members
    '[ Reader SrcSpan,
       Writer (SrcSpan, Errors),
       Writer (TypeVariable, SrcSpan),
       Fresh
     ]
    r =>
  AttrSetPath ->
  Scheme ->
  Eff r Scheme
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
      Scheme ->
      Eff r Scheme
    go [] x = return x
    go (p : ps) x = do
      expectAttrSet x <&> M.lookup p >>= \case
        Nothing -> scheme . NTypeVariable <$> throwSrcTV (KeyNotPresent p)
        Just t -> go ps t
