module Tix.Typechecker
  ( infer,
    solve,
    runFresh,
    runSeqWriter,
    getType,
    runNoTreeTracer,
    renderPretty,
  )
where

import Control.Lens hiding (Empty, List, cons, equality, set, set')
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
import Data.Functor.Contravariant
import Data.Group hiding ((~~))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.MultiKey.Strict as MKM
import Data.Map.Shifted.Strict (ShiftedMap (..))
import qualified Data.Map.Shifted.Strict as SM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
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

getType :: NExprLoc -> (Scheme, [(SrcSpan, Errors)], [InferError], [UnifyingError], [PredicateError], A.Object)
getType r =
  let ((((((((t, traceTree), inf), predicate), unifying), errs), _srcs), subs), _) =
        run
          . evalState @(MKM.Map Pred) mempty
          . interpret @(Writer Pred) (\(Tell x) -> modify (<> MKM.singleton x))
          . evalState (M.empty :: VariableMap)
          . runConstraintEnv
          . runState @[(TypeVariable, SrcSpan)] []
          . interpret @(Writer (TypeVariable, SrcSpan)) (\(Tell x) -> modify (x :))
          . runSeqWriter @[(SrcSpan, Errors)]
          . runSeqWriter @[UnifyingError]
          . runSeqWriter @[PredicateError]
          . runSeqWriter @[InferError]
          . runFresh
          . runTreeTracer
          $ inferGeneral r
   in (sub subs t, errs, inf, unifying, predicate, traceTree)

runConstraintEnv ::
  Members '[State VariableMap, State (MKM.Map Pred)] effs =>
  Eff (SubstituteEnv : Writer Substitution : Writer Constraint : State (Seq Constraint) : effs) a ->
  Eff effs ((a, Substitution), Seq Constraint)
runConstraintEnv =
  runState @(Seq Constraint) Empty
    . interpret @(Writer Constraint) (\(Tell x) -> modify (x :<|))
    . runWriter @Substitution
    . interpret @SubstituteEnv
      ( \(SubstituteEnv x) -> do
          modify @(Seq Constraint) (sub x <$>)
          -- TODO: I have a feeling that variables in the env should already be closed over.
          -- (aka this is redundant)
          modify @VariableMap (sub x <$>)
          modify @(MKM.Map Pred) (sub x)
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

class Free s where
  free :: s -> Set TypeVariable
  sub :: Substitution -> s -> s

instance Free NType where
  free (NTypeVariable v) = S.singleton v
  free (List x) = free x
  free (NAttrSet attrs) = foldMap free . M.elems $ attrs
  free (x :-> y) = free x <> free y
  free (NBruijn _) = S.empty
  free (NAtomic _) = S.empty

  sub s r = case r of
    NBruijn _ -> r
    NTypeVariable x -> case M.lookup x $ unSubstitution s of
      Nothing -> NTypeVariable x
      Just t -> t
    List x -> List $ sub s x
    NAttrSet m -> NAttrSet $ sub s <$> m
    x :-> y -> sub s x :-> sub s y
    x@(NAtomic _) -> x

instance Free Preds where
  free (Preds ps) = flip foldMap ps $ \case
    Update x y z -> free x <> free y <> free z
    x `HasField` (_, y) -> free x <> free y
  sub s (Preds ps) =
    Preds $
      ps <&> \case
        Update x y z -> Update (sub s x) (sub s y) (sub s z)
        HasField t (f, v) -> HasField (sub s t) (f, sub s v)

instance Free Scheme where
  free (x :=> y) = free x <> free y
  sub s (x :=> y) = sub s x :=> sub s y

instance Free a => Free [a] where
  free = foldMap free
  sub s = fmap (sub s)

instance Free Substitution where
  free (Substitution s) = free . M.elems $ s
  sub new old = old <> new

instance (Free a, Free b) => Free (a, b) where
  free (a, b) = free a <> free b
  sub s (a, b) = (sub s a, sub s b)

instance Free (MKM.Map Pred) where
  free = MKM.keys
  sub subs@(Substitution s) mkm@(MKM.Map m _) = m'
    where
      vals = S.toList . fold . M.elems $ M.restrictKeys m (M.keysSet s)
      f = appEndo $ foldMap (Endo . MKM.delete) vals
      m' = f mkm <> MKM.fromList (unPreds . sub subs $ Preds vals)

newtype Substitution = Substitution {unSubstitution :: Map TypeVariable NType}
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
  NType ->
  UnifyM Substitution
v <<- (NTypeVariable x) | v == x = return mempty
v <<- x | v `occursIn` x = tell (InfinityType x) >> return mempty
  where
    occursIn :: Free x => TypeVariable -> x -> Bool
    occursIn t u = t `S.member` free u
v <<- x = return $ Substitution $ M.singleton v x

unifyWithPriority ::
  -- | The range of type variables to interpret as being lower
  -- priority (try to replace them if possible)
  Predicate TypeVariable ->
  Constraint ->
  UnifyM Substitution
unifyWithPriority f (lhs :~ rhs) =
  case (lhs, rhs) of
    (NAtomic x, NAtomic y) | x == y -> return mempty
    (NTypeVariable x, NTypeVariable y) -> do
      if getPredicate f y
        then y <<- NTypeVariable x
        else x <<- NTypeVariable y
    (NTypeVariable t, x) -> t <<- x
    (x, NTypeVariable t) -> t <<- x
    (List x, List y) -> unifyWithPriority f $ x :~ y
    (NAttrSet _x, NAttrSet _y) -> error $ "TODO: This should be handled in a special way " <> show _x <> show _y
    -- foldr (\c s -> s >>= \s' -> (<> s') <$> unifyWithPriority f (sub s' c)) (pure mempty)
    --   . M.elems
    --   $ M.intersectionWith (:~) x y
    (x1 :-> y1, x2 :-> y2) -> do
      s <- unifyWithPriority f (x1 :~ x2)
      s' <- unifyWithPriority f $ sub s $ y1 :~ y2
      return $ s' <> s
    (x, y) -> tell (CanNotUnify x y) >> return mempty

solve :: Seq Constraint -> UnifyM Substitution
solve = solveWithPriority mempty

solveWithPriority ::
  -- | The range of type variables to interpret as being lower
  -- priority (try to replace them if possible)
  Predicate TypeVariable ->
  Seq Constraint ->
  UnifyM Substitution
solveWithPriority _ Empty = return mempty
solveWithPriority f (rest :|> c) = do
  s <- unifyWithPriority f c
  -- TODO: This should probably be (s <>)
  (<> s) <$> solveWithPriority f (fmap (sub s) rest)

data Errors
  = UndefinedVariable VarName
  | UnexpectedType {expected :: Scheme, got :: Scheme}
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

data PredicateError
  = KeyNotPresent Text NType
  | NotAnAttributeSet NType
  deriving stock (Eq, Show)

data InferError
  = ConflictingBindingDefinitions NormalizedBinding' NormalizedBinding'
  deriving stock (Show)

type UnifyM x = forall r. Members '[Writer UnifyingError] r => Eff r x

type InferEffs =
  '[ Fresh,
     Writer (TypeVariable, SrcSpan),
     Writer Constraint,
     Writer (SrcSpan, Errors),
     Writer PredicateError,
     Writer Pred,
     State (MKM.Map Pred),
     State VariableMap,
     Writer UnifyingError,
     Writer InferError,
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

(~~) :: Member (Writer Constraint) r => NType -> NType -> Eff r ()
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
  Members '[Fresh, Reader SrcSpan, Writer (TypeVariable, SrcSpan), Writer Pred] effs =>
  Scheme ->
  Eff effs NType
instantiate s = do
  (Preds cs :=> t) <- runReader 0 . evalState M.empty . instantiateScheme $ s
  tell `traverse_` cs
  return t
  where
    instantiateScheme :: forall. Scheme -> Eff (State (Map DeBruijn TypeVariable) ': Reader Int ': effs) Scheme
    instantiateScheme =
      traverseNTypesWith (local @Int (+ 1)) %%~ \case
        (TBruijn db) -> TTypeVariable <$> freshInst db
        x@(TAtomic _) -> return x
        x@(TTypeVariable _) -> return x

    freshInst :: DeBruijn -> Eff (State (Map DeBruijn TypeVariable) ': Reader Int ': effs) TypeVariable
    freshInst (DeBruijn i j) = do
      x <- ask @Int
      let db = DeBruijn (i - x) j
      gets (M.lookup db) >>= \case
        Just t -> return t
        Nothing -> do
          t <- freshSrc
          modify (M.insert db t)
          return t

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
      >>= maybe (scheme . NTypeVariable <$> throwSrcTV (UndefinedVariable var)) return . M.lookup var
      >>= instantiate -- I have no idea if this covers all cases.
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
    res <- NTypeVariable <$> freshSrc
    tell (lhsT // rhsT $ res)
    return res
  NBinary NApp lhs rhs -> do
    lhst <- infer lhs
    rhst <- infer rhs
    rest <- NTypeVariable <$> freshSrc
    lhst ~~ (rhst :-> rest)
    return rest
  NBinary NConcat lhs rhs -> do
    lhst <- infer lhs
    rhst <- infer rhs
    t <- NTypeVariable <$> freshSrc
    let listt = List t
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
    let path = getPath path'
    resT <-
      NTypeVariable <$> case path of
        Just p -> do
          (set', var) <- lookupAttrSet p
          setT ~~ set'
          return var
        Nothing -> freshSrc
    case def of
      Nothing -> return ()
      Just defR -> do
        -- this should infer optional type
        defT <- infer defR
        defT ~~ resT
    return resT
  NSet NNonRecursive xs -> NAttrSet <$> inferBinding xs
  NHasAttr attrSet path -> do
    setT <- infer attrSet
    -- this should infer optional type
    return setT
  NAbs param body -> do
    (paramT, varMap :: VariableMap) <- case param of
      Param name -> do
        t <- freshSrc
        return (NTypeVariable t, M.singleton name . scheme $ NTypeVariable t)
      ParamSet set variadic binding -> do
        setBindings <-
          M.fromList <$> set
            `for` ( \(name, def) -> do
                      t <- case def of
                        Nothing -> NTypeVariable <$> freshSrc
                        Just def' -> infer def'
                      return (name, scheme t) -- TODO: This is bad: it probably shouldn't be a scheme
                  )
        let setT = NAttrSet $ setBindings
        return (setT, maybe M.empty (`M.singleton` scheme setT) binding <> setBindings)
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
  -- NWith set body -> do
  --   -- TODO: implement fallback variable lookup -> specify that the set should have the key.
  --   setT <- infer set
  --   vars <- expectAttrSet setT
  --   withBindings vars $ infer body
  --   where
  --     -- Deprecated
  --     expectAttrSet ::
  --       Members '[Reader SrcSpan, Writer (SrcSpan, Errors)] r =>
  --       Scheme ->
  --       Eff r VariableMap
  --     expectAttrSet (_ :=> NAttrSet x) = return x
  --     expectAttrSet g = do
  --       throwSrc
  --         UnexpectedType
  --           { expected = scheme $ NAttrSet M.empty,
  --             got = g
  --           }
  --       return M.empty
  NAssert cond body -> do
    condT <- infer cond
    condT ~~ NAtomic Bool
    infer body
  NSynHole _ -> NTypeVariable <$> freshSrc
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

data NormalizedBinding' = NBType !NExprLoc | NBAttrSet !NormalizedBinding
  deriving stock (Show)

type NormalizedBinding = Map VarName NormalizedBinding'

normalizeBindings :: Member (Writer InferError) eff => [Binding NExprLoc] -> Eff eff NormalizedBinding
normalizeBindings bindings = case bindings' of
  [] -> return M.empty
  (x : xs) ->
    sequence $
      -- This is really sub-optimal
      foldl (\u -> M.unionWith (\i j -> i >>= \i' -> j >>= mergeBindings i') u . fmap pure) (fmap pure x) xs
  where
    mergeBindings (NBAttrSet lhs) (NBAttrSet rhs) =
      fmap NBAttrSet . sequence $
        -- This is really sub-optimal
        M.unionWith (\x y -> x >>= \x' -> y >>= mergeBindings x') (pure <$> lhs) (pure <$> rhs)
    mergeBindings x y = do
      tell $ ConflictingBindingDefinitions x y
      return x
    fromName :: NonEmpty VarName -> NExprLoc -> NormalizedBinding
    fromName (n :| ns) x = M.singleton n $ foldr (\u t -> NBAttrSet $ M.singleton u t) (NBType x) ns
    bindings' =
      bindings >>= \case
        (NamedVar path r src) ->
          case getPath path of
            Nothing -> [] -- TODO: This should be an error (?)
            Just name -> pure $ fromName name r
        (Inherit attrSet keys src) ->
          let names :: [VarName] = mapMaybe getKeyName keys
              f :: VarName -> NExprLoc = case attrSet of
                Nothing -> \v -> Fix $ Compose $ Ann (SrcSpan src src) $ NSym v
                Just set -> \v -> Fix $ Compose $ Ann (SrcSpan src src) $ NSelect set (pure $ StaticKey v) Nothing
           in (\v -> fromName (pure v) (f v)) <$> names

inferNormalizedBindings' :: NormalizedBinding' -> InferM' Scheme
inferNormalizedBindings' (NBType expr) = inferGeneral expr
inferNormalizedBindings' (NBAttrSet m) = scheme . NAttrSet <$> inferNormalizedBindings' `traverse` m

inferNormalizedBindings :: NormalizedBinding -> InferM' (Map VarName Scheme)
inferNormalizedBindings = traverse inferNormalizedBindings'

inferBinding :: [Binding NExprLoc] -> InferM' (Map VarName Scheme)
inferBinding = normalizeBindings >=> inferNormalizedBindings

renderPretty :: Pretty x => x -> TL.Text
renderPretty = renderLazy . layoutPretty defaultLayoutOptions . pretty

showExpr :: Fix NExprLocF -> Text
showExpr = TL.toStrict . renderLazy . layoutPretty defaultLayoutOptions . P.group . prettyNix . stripAnnotation

inferGeneral :: NExprLoc -> InferM Scheme
inferGeneral x@(Fix (Compose (Ann src _))) = runReader src $
  traceSubtree (showExpr x) $ do
    (((t, subs), cs), range) <-
      traceSubtree "subexpressions" . registerTypeVariables . runConstraintEnv $ infer x
    traceSubtree "received" $ do
      traceValue "type" $ renderPretty t
      traceValue "substitutions" $ prettySubstitution subs
      traceValue "constraints" $ fmap renderPretty cs
      traceValue "range" $ T.pack . show $ range
      get @(MKM.Map Pred) >>= traceValue "preds" . showPreds
    s' <- solveWithPriority (Predicate (`RS.member` range)) cs
    traceValue "substitutions_after_solving" $ prettySubstitution s'
    let s = subs <> s'
        returnedS = Substitution . M.filterWithKey (\k _ -> k `RS.notMember` range) . unSubstitution $ s
    traceSubtree "returned" $ do
      traceValue "substitutions" $ prettySubstitution returnedS
      -- Filter for optimization purposes
      subEnv returnedS
      preds <- get @(MKM.Map Pred)
      traceValue "subbed_preds" $ showPreds preds
      let p = Predicate (`RS.member` range) <> Predicate (`S.notMember` free returnedS)
          (MKM.toList -> keep, toss) = MKM.partition (getPredicate p) preds
      -- At this point the only predicates only contain type variables that are only used
      -- in the expression. This means that all of the generated constraints and
      -- substitutions can be safely restricted to this expression.
      retT <- solveAndClose p $ sub s $ Preds keep :=> t
      traceValue "preds" $ showPreds toss
      put toss
      traceValue "type" $ renderPretty retT
      return retT
  where
    showPreds :: MKM.Map Pred -> Map String String
    showPreds = M.mapKeys show . fmap show . MKM.toMap

solveAndClose ::
  Members
    '[ Writer (TypeVariable, SrcSpan),
       Reader SrcSpan,
       Fresh,
       Writer PredicateError,
       Writer UnifyingError
     ]
    eff =>
  Predicate TypeVariable ->
  Scheme ->
  Eff eff Scheme
solveAndClose p (cs :=> t) = do
  (((Preds solvedPreds, range), constraints), collectedPreds) <-
    runSeqWriter @[Pred]
      . runSeqWriter @(Seq Constraint)
      . registerTypeVariables
      $ solveConstraints cs
  let newPredicate = p `pOr` Predicate (`RS.member` range)
  case (constraints, collectedPreds) of
    (Empty, []) -> return . close newPredicate $ Preds solvedPreds :=> t
    _ -> do
      s <- solveWithPriority newPredicate constraints
      solveAndClose newPredicate $ sub s $ Preds (collectedPreds <> solvedPreds) :=> t

pOr :: Predicate x -> Predicate x -> Predicate x
pOr (Predicate f) (Predicate g) = Predicate $ \x -> f x || g x

solveConstraints ::
  Members
    '[ Fresh,
       Reader SrcSpan,
       Writer (TypeVariable, SrcSpan),
       Writer Pred,
       Writer PredicateError,
       Writer Constraint
     ]
    effs =>
  Preds ->
  Eff effs Preds
solveConstraints (Preds ps) =
  Preds <$> do
    flip Control.Monad.filterM ps $ \case
      HasField (NAttrSet ts) (f, t) -> case M.lookup f ts of
        Nothing -> do
          tell $ KeyNotPresent f (NAttrSet ts)
          return False
        Just t' -> do
          x <- instantiate t
          y <- instantiate t'
          x ~~ y
          return False
      HasField (NTypeVariable _) _ -> return True
      HasField t _ -> do
        tell $ NotAnAttributeSet t
        return False
      (Update (NAttrSet a) (NAttrSet b) t) -> do
        (NAttrSet $ b <> a) ~~ t
        return False
      _ -> return True

newtype DeBruijnContext = DeBruijnContext (Sum Int)
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, Group, Num)

instance Act DeBruijnContext DeBruijn where
  act (DeBruijnContext (Sum n)) (DeBruijn i j) = DeBruijn (i + n) j

type TDeBruijnMap = ShiftedMap DeBruijnContext TypeVariable DeBruijn

close :: TraversableNTypes x => Predicate TypeVariable -> x -> x
close (Predicate f) =
  run
    . evalState @TDeBruijnMap mempty
    . runReader @Int 0
    . evalState @Int 0
    . close'
  where
    -- State is the sequential number, Reader is the binding context number
    -- close' :: NType -> Eff '[State Int, Reader Int, State TDeBruijnMap] NType
    close' =
      traverseNTypesWith bndCtx %%~ \case
        x@(TBruijn _) -> return x
        x@(TAtomic _) -> return x
        (TTypeVariable tv) | f tv -> TBruijn <$> newbruijn tv
        x@(TTypeVariable _) -> return x

    bndCtx m = do
      modify @TDeBruijnMap (SM.shift 1)
      x <- local @Int (+ 1) m
      modify @TDeBruijnMap (SM.shift (-1))
      return x
    newbruijn t = do
      get @TDeBruijnMap <&> SM.lookup t >>= \case
        Nothing -> do
          i <- ask
          j <- get
          modify @Int (+ 1)
          let x = DeBruijn i j
          modify @TDeBruijnMap (SM.insert t x)
          return x
        Just x -> return x

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

getKeyName :: NKeyName r -> (Maybe VarName)
getKeyName (StaticKey k) = Just k
getKeyName (DynamicKey k) = antiAntiQuote k

getPath ::
  (Traversable t) =>
  t (NKeyName r) ->
  Maybe (t Text)
getPath = mapM getKeyName

type AttrSetPath = NonEmpty Text

lookupAttrSet ::
  Members
    '[ Reader SrcSpan,
       Writer (SrcSpan, Errors),
       Writer (TypeVariable, SrcSpan),
       Writer Pred,
       Fresh
     ]
    r =>
  AttrSetPath ->
  Eff r (NType, TypeVariable)
lookupAttrSet (p :| []) = do
  set <- NTypeVariable <$> freshSrc
  var <- freshSrc
  tell (HasField set (p, scheme . NTypeVariable $ var))
  return (set, var)
lookupAttrSet (p :| (pp : ps)) = do
  set <- NTypeVariable <$> freshSrc
  (innerSet, var) <- lookupAttrSet (pp :| ps)
  tell $ HasField set (p, scheme innerSet)
  return (set, var)
