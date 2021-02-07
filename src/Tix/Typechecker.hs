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
import Control.Monad.Freer.Internal (handleRelayS)
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Data.Act
import qualified Data.Aeson as A
import Data.Fix
import Data.Foldable
import Data.Functor
import Data.Group hiding ((~~))
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Shifted.Strict (ShiftedMap (..))
import qualified Data.Map.Shifted.Strict as SM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.MonoTraversable
import Data.Monoid
import Data.Range (Range (..))
import qualified Data.Range as R
import Data.Sequence (Seq (..))
import Data.Sequences
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T
import Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Traversable
import qualified Data.Vector as V
import Nix.Atoms
import Nix.Expr
import Nix.Pretty

runConstraintEnv ::
  Eff (SubstituteEnv : Writer Substitution : Writer Constraint : State (Seq Constraint) : effs) a ->
  Eff effs ((a, Substitution), Seq Constraint)
runConstraintEnv =
  runState @(Seq Constraint) Empty
    . interpret @(Writer Constraint) (\(Tell x) -> modify (x :<|))
    . runWriter @Substitution
    . interpret @(SubstituteEnv)
      ( \(SubstituteEnv x) -> do
          modify @(Seq Constraint) (subCon x <$>)
          tell x
          return ()
      )

getType :: NExprLoc -> (NType, [(SrcSpan, Errors)], [UnifyingError], A.Object)
getType r =
  let ((((((t, traceTree), unifying), errs), srcs), subs), _) =
        run
          . runConstraintEnv
          . runState @[(TypeVariable, SrcSpan)] []
          . interpret @(Writer (TypeVariable, SrcSpan)) (\(Tell x) -> modify (x :))
          . runSeqWriter @[(SrcSpan, Errors)]
          . runSeqWriter @[UnifyingError]
          . runReader (M.empty :: VariableMap)
          . runFresh
          . runTreeTracer
          $ inferGeneral r
   in (sub subs t, errs, unifying, traceTree)

data SubstituteEnv a where
  SubstituteEnv :: Substitution -> SubstituteEnv ()

subEnv :: Member SubstituteEnv r => Substitution -> Eff r ()
subEnv = send . SubstituteEnv

newtype TypeVariable = TypeVariable Int
  deriving newtype (Eq, Ord)

instance Show TypeVariable where
  show (TypeVariable n) = "〚" <> show n <> "〛"

data Constraint = !NType :~ !NType
  deriving stock (Eq, Ord, Show)

instance Pretty Constraint where
  pretty (x :~ y) = pretty x <+> "~" <+> pretty y

data DeBrujin = DeBrujin !Int !Int
  deriving stock (Show, Eq, Ord)

greek :: [T.Builder]
greek = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]

variableNames :: [T.Builder]
variableNames =
  greek
    <> (fmap (\(n, g) -> g <> T.fromString (show n)) $ zip [1 :: Int ..] greek)

showNType :: NType -> Text
showNType t = TL.toStrict . T.toLazyText $ showNType' 0 t
  where
    greekVars = M.fromList $ zip (S.toList $ getAllDeBurjins t) variableNames
    showNType' :: Int -> NType -> T.Builder
    showNType' depth u =
      foralls <> case u of
        (NAtomic a) -> showAtomicType a
        (NBrujin (DeBrujin i j)) -> greekVars M.! (DeBrujin (i - depth) j)
        (NTypeVariable _) -> error "should not have free type variables at this point"
        (x :-> y) -> showNextNType' x <> " -> " <> showNextNType' y
        (List x) -> "[" <> showNextNType' x <> "]"
        (NAttrSet x) ->
          "{ "
            <> ( mconcat
                   . intersperse "; "
                   . fmap (\(k, v) -> T.fromText k <> " = " <> showNextNType' v)
                   $ M.toList x
               )
            <> " }"
      where
        showNextNType' = showNType' $ depth + 1
        currBurjins = S.mapMonotonic (DeBrujin (- depth)) $ getDeBurjins u
        foralls =
          if S.null currBurjins
            then ""
            else
              "∀ "
                <> (mconcat . intersperse " " . M.elems $ M.restrictKeys greekVars currBurjins)
                <> ". "

showAtomicType :: AtomicType -> T.Builder
showAtomicType Integer = "Integer"
showAtomicType Float = "Float"
showAtomicType Bool = "Bool"
showAtomicType String = "String"
showAtomicType Path = "Path"
showAtomicType Null = "Null"
showAtomicType URI = "URI"

data NType
  = NTypeVariable !TypeVariable
  | NBrujin !DeBrujin
  | NAtomic !AtomicType
  | -- These two introduce a new De Brujin binding context
    !NType :-> !NType
  | List !NType
  | NAttrSet !(Map Text NType)
  deriving stock (Eq, Ord, Show)

data AtomicType
  = Integer
  | Float
  | Bool
  | String
  | Path
  | Null
  | URI
  deriving stock (Eq, Ord, Show)

-- | Get all De Brujin type variables that were bound in the the outermost
-- (current) binding context.
-- Only returns the second indexes.
getDeBurjins :: NType -> Set Int
getDeBurjins = getDeBurjins' 0

-- | Gets all De Brujin variables with the given binding context offset.
-- Only returns the second indexes.
getDeBurjins' :: Int -> NType -> Set Int
getDeBurjins' _ (NTypeVariable _) = S.empty
getDeBurjins' _ (NAtomic _) = S.empty
getDeBurjins' n (NBrujin (DeBrujin m j)) | m == n = S.singleton j
getDeBurjins' _ (NBrujin _) = S.empty
getDeBurjins' n (x :-> y) = getDeBurjins' (n + 1) x <> getDeBurjins' (n + 1) y
getDeBurjins' n (List x) = getDeBurjins' (n + 1) x
getDeBurjins' n (NAttrSet xs) = foldMap (getDeBurjins' (n + 1)) . M.elems $ xs

-- | Returns the set of all De Brujin type variables relative to the outermost
-- (current) binding context.
-- So variables that are bound in one of the inner contexts will have a negative
-- binding context number.
getAllDeBurjins :: NType -> Set DeBrujin
getAllDeBurjins =
  \case
    (NTypeVariable _) -> S.empty
    (NAtomic _) -> S.empty
    (NBrujin x) -> S.singleton x
    (x :-> y) -> bndCtx $ getAllDeBurjins x <> getAllDeBurjins y
    (List x) -> bndCtx $ getAllDeBurjins x
    (NAttrSet xs) -> bndCtx . foldMap getAllDeBurjins . M.elems $ xs
  where
    bndCtx = S.mapMonotonic (\(DeBrujin i j) -> DeBrujin (i - 1) j)

newtype DeBrujinContext = DeBrujinContext (Sum Int)
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, Group, Num)

instance Act DeBrujinContext DeBrujin where
  act (DeBrujinContext (Sum n)) (DeBrujin i j) = DeBrujin (i + n) j

type TDeBrujinMap = ShiftedMap DeBrujinContext TypeVariable DeBrujin

close :: Range TypeVariable -> NType -> NType
close range =
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
    close' (NTypeVariable tv) | tv `R.member` range = NBrujin <$> newbrujin tv
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

instance Pretty NType where
  pretty (NTypeVariable t) = pretty $ prettyTypeVariable t
  pretty (NBrujin (DeBrujin i j)) = "⟦" <> pretty i <+> pretty j <> "⟧"
  pretty (NAtomic a) = viaShow a
  pretty (x :-> y) = sep [pretty x, "->" <+> pretty y]
  pretty (List x) = "[" <> align (pretty x) <> "]"
  pretty (NAttrSet x) =
    sep
      [ flatAlt "{ " "{"
          <+> align
            ( sep . zipWith (<+>) ("" : repeat ";")
                . map (\(k, v) -> pretty k <+> "=" <+> align (pretty v))
                $ M.toList x
            ),
        "}"
      ]

newtype Substitution = Substitution {unSubstitution :: Map TypeVariable NType}
  deriving newtype (Eq, Ord, Show)

prettyTypeVariable :: TypeVariable -> TL.Text
prettyTypeVariable (TypeVariable n) = "⟦" <> (TL.pack . show) n <> "⟧"

prettySubstitution :: Substitution -> A.Value
prettySubstitution = A.toJSON . M.mapKeysMonotonic prettyTypeVariable . fmap renderPretty . unSubstitution

-- | Prefers the left
instance Semigroup Substitution where
  x'@(Substitution x) <> (Substitution y) = Substitution (x <> fmap (sub x') y)

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
unify = unifyWithPriority EmptyRange

unifyWithPriority ::
  -- | The range of type variables to interpret as being lower
  -- priority (try to replace them if possible)
  Range TypeVariable ->
  Constraint ->
  UnifyM Substitution
unifyWithPriority range (lhs :~ rhs) =
  case (lhs, rhs) of
    (NAtomic x, NAtomic y) | x == y -> return mempty
    (NTypeVariable x, NTypeVariable y) ->
      if y `R.member` range
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
solve = solveWithPriority EmptyRange

solveWithPriority ::
  -- | The range of type variables to interpret as being lower
  -- priority (try to replace them if possible)
  Range TypeVariable ->
  Seq Constraint ->
  UnifyM Substitution
solveWithPriority _ Empty = return mempty
solveWithPriority range (rest :|> c) = do
  s <- unifyWithPriority range c
  (<> s) <$> solveWithPriority range (fmap (subCon s) rest)

-- pattern Sempty <- (S.null -> True) where Sempty = S.empty

occursIn :: Free x => TypeVariable -> x -> Bool
occursIn t x = t `S.member` free x

data Errors
  = UndefinedVariable VarName
  | UnexpectedType {expected :: NType, got :: NType}
  | KeyNotPresent Text
  deriving stock (Eq, Ord, Show)

data Fresh x where
  Fresh :: Fresh TypeVariable
  -- | Inclusive
  MinFreshFromNow :: Fresh TypeVariable

type Runner effect = forall effs a. Eff (effect ': effs) a -> Eff effs a

runFresh :: Runner Fresh
runFresh =
  evalState (TypeVariable 0)
    . reinterpret
      ( \case
          Fresh -> do
            (TypeVariable x) <- get
            put (TypeVariable $ x + 1)
            return (TypeVariable x)
          MinFreshFromNow -> get
      )

registerTypeVariables :: Member Fresh r => Eff r a -> Eff r (a, Range TypeVariable)
registerTypeVariables m = do
  l <- send MinFreshFromNow
  a <- m
  r <- send MinFreshFromNow
  return (a, Range l r)

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
     Reader VariableMap,
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
  (Member (Reader r) effs, Semigroup r) =>
  r ->
  Eff effs a ->
  Eff effs a
withBindings bindings = local (bindings <>)

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
    ask @VariableMap
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
          Nothing -> ask @VariableMap
        names <- catMaybes <$> traverse getKeyName keys
        return $
          catMaybes $
            names <&> \name -> case M.lookup name vars of
              Nothing -> Nothing
              Just r -> Just (name, r)
  return $ M.fromListWith mergeSetTypes bindings'

-- traceShowMStack :: (HasCallStack, Applicative f, Show a) => a -> f ()
-- traceShowMStack = withFrozenCallStack $ traceShowMStack' ""

-- traceShowMStack' :: (HasCallStack, Applicative f, Show a) => String -> a -> f ()
-- traceShowMStack' s a = withFrozenCallStack $ do
--   case getCallStack callStack of
--     (_ : (x, _) : _) -> traceM $ x <> " " <> s <> " \t" <> show a
--     _ -> traceM $ s <> " \t" <> show a

renderPretty :: Pretty x => x -> TL.Text
renderPretty = renderLazy . layoutPretty defaultLayoutOptions . pretty

showExpr :: Fix NExprLocF -> Text
showExpr = TL.toStrict . renderLazy . layoutPretty defaultLayoutOptions . P.group . prettyNix . stripAnnotation

inferGeneral :: NExprLoc -> InferM NType
inferGeneral x = traceSubtree (showExpr x) $ do
  (((t, subs), cs), range) <-
    traceSubtree "subexpressions" . registerTypeVariables . runConstraintEnv $ infer x
  traceValue "received_type" $ renderPretty t
  traceValue "received_substitutions" $ prettySubstitution subs
  traceValue "received_constraints" $ fmap renderPretty cs
  s' <- solveWithPriority range cs
  traceValue "substitutions_after_solving" $ prettySubstitution s'
  let s = subs <> s'
      returnedS = Substitution . M.filterWithKey (\k _ -> k `R.notMember` range) . unSubstitution $ s
  traceValue "returned_substitutions" $ prettySubstitution returnedS
  -- Filter for optimization purposes
  subEnv returnedS
  let retT = close range $ sub s t
  traceValue "returned_type" $ renderPretty retT
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

data TreeTracer a where
  TraceTree :: A.Object -> TreeTracer ()

traceValue :: (A.ToJSON v, Member TreeTracer eff) => Text -> v -> Eff eff ()
traceValue k v = send $ TraceTree $ k A..= v

traceSubtree :: Member TreeTracer eff => Text -> Eff eff a -> Eff eff a
traceSubtree k = interpose (\(TraceTree o) -> send $ TraceTree (k A..= o))

runTreeTracer :: Eff (TreeTracer ': eff) a -> Eff eff (a, A.Object)
runTreeTracer = handleRelayS HM.empty (\s x -> pure (x, s)) $ \s (TraceTree o) k ->
  k (HM.unionWith unionToList s o) ()
  where
    unionToList :: A.Value -> A.Value -> A.Value
    unionToList (A.Array x) (A.Array y) = A.Array $ x <> y
    unionToList (A.Array x) y = A.Array $ V.snoc x y
    unionToList x (A.Array y) = A.Array $ V.cons x y
    unionToList (A.Object x) (A.Object y) = A.Object $ HM.unionWith unionToList x y
    unionToList x y = A.Array $ V.fromList [x, y]

runNoTreeTracer :: Eff (TreeTracer ': eff) a -> Eff eff a
runNoTreeTracer = interpret (\(TraceTree _) -> return ())
