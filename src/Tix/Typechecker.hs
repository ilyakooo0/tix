module Tix.Typechecker
  ( infer,
    solve,
    runFresh,
    runSeqWriter,
    getType,
  )
where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Internal (handleRelayS)
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Data.Fix
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.MonoTraversable
import Data.Sequence (Seq (..))
import Data.Sequences
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Traversable
import Nix.Atoms
import Nix.Expr

getType :: NExprLoc -> (Either UnifyingError NTypeS, [(SrcSpan, Errors)])
getType exp =
  let (((t, srcs), cons), errs) =
        run
          . runReader (M.empty :: VariableMap)
          . runSeqWriter @[(SrcSpan, Errors)]
          . runSeqWriter @(Seq Constraint)
          . runSeqWriter @[(TypeVariable, SrcSpan)]
          . runFresh
          $ infer exp
      s = run . runError @UnifyingError $ solve cons
   in ((`sub` t) <$> s, errs)

newtype TypeVariable = TypeVariable Int
  deriving newtype (Show, Eq, Ord)

data Constraint = !NTypeS :~ !NTypeS

type NType = Fix NTypeF

data NTypeF f
  = NTypeVariable !TypeVariable
  | NAtomic !AtomicType
  | List !f
  | NAttrSet !(Map Text f)
  | !f :-> !f
  deriving stock (Functor, Eq, Ord, Show)

instance Show1 NTypeF where
  liftShowsPrec s _ p (NAtomic a) = (show a <>)
  liftShowsPrec s _ p (NTypeVariable a) = (show a <>)
  liftShowsPrec s _ p (List a) = ("[" <>) <> s p a <> ("]" <>)
  liftShowsPrec s _ p (NAttrSet a) = ("{" <>) <> x <> ("}" <>)
    where
      x =
        mconcat . fmap (\(k, v) -> (show k <>) <> (" :: " <>) <> s p v) $
          M.toList a
  liftShowsPrec s _ p (a :-> b) = s p a <> (" -> " <>) <> s p b

instance Eq1 NTypeF where
  liftEq _ (NTypeVariable x) (NTypeVariable y) = x == y
  liftEq _ (NAtomic x) (NAtomic y) = x == y
  liftEq eq (List x) (List y) = eq x y
  liftEq eq (NAttrSet x) (NAttrSet y) = liftEq eq x y
  liftEq eq (x1 :-> y1) (x2 :-> y2) = eq x1 x2 && eq y1 y2
  liftEq _ _ _ = False

data AtomicType
  = Integer
  | Float
  | Bool
  | String
  | Path
  | Null
  | URI
  deriving stock (Eq, Ord, Show)

class Free s where
  free :: s -> Set TypeVariable

instance Free f => Free (NTypeF f) where
  free (NTypeVariable v) = S.singleton v
  free (NAtomic _) = S.empty
  free (List x) = free x
  free (NAttrSet attrs) = foldMap free . M.elems $ attrs
  free (x :-> y) = free x <> free y

instance Free (f (Fix f)) => Free (Fix f) where
  free (Fix f) = free f

instance (Free (f (g x))) => Free (Compose f g x) where
  free (Compose x) = free x

instance Free f => Free (SchemeX f) where
  free (Scheme vs x) = free x `S.difference` vs

newtype Substitution = Substitution (Map TypeVariable NTypeS)

instance Semigroup Substitution where
  x'@(Substitution x) <> (Substitution y) = Substitution (x <> fmap (sub x') y)

instance Monoid Substitution where
  mempty = Substitution mempty

sub :: Substitution -> NTypeS -> NTypeS
sub (Substitution s) r'@(Fix (Compose (Scheme tvs r))) = case r of
  NAtomic _ -> r'
  NTypeVariable x -> subTV x
  List x -> rewrap . List $ sub' x
  NAttrSet m -> rewrap . NAttrSet $ sub' <$> m
  x :-> y -> rewrap $ sub' x :-> sub' y
  where
    s' = s `M.withoutKeys` tvs
    subTV x = M.findWithDefault r' x s'
    sub' = sub $ Substitution s'
    rewrap = Fix . Compose . Scheme tvs

subCon :: Substitution -> Constraint -> Constraint
subCon s (x :~ y) = sub s x :~ sub s y

(<<-) ::
  TypeVariable ->
  NTypeF (NTypeS) ->
  UnifyM Substitution
v <<- (NTypeVariable x) | v == x = return mempty
v <<- x | v `occursIn` x = throwError $ InfinityType $ normalType x
v <<- x = return $ Substitution $ M.singleton v $ normalType x

unify :: Constraint -> UnifyM Substitution
unify (Fix (Compose (Scheme Sempty lhs')) :~ Fix (Compose (Scheme Sempty rhs'))) =
  case (lhs', rhs') of
    (NAtomic x, NAtomic y) | x == y -> return mempty
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
    (x, y) -> throwError $ CanNotUnify x y

solve :: Seq Constraint -> UnifyM Substitution
solve Empty = return mempty
solve (rest :|> c) = do
  s <- unify c
  solve $ fmap (subCon s) rest

-- type NormalizeM = Eff '[State (Map TypeVariable TypeVariable), State (Set TypeVariable), Fresh]

-- normalize :: NTypeS -> NormalizeM NTypeS
-- normalize (Fix (Compose u)) =
--   Fix . Compose <$> case u of
--     (Scheme tvs x) ->

-- normalize :: SchemeX (NTypeF NTypeS) -> SchemeX (NTypeF NTypeS)
-- normalize (Scheme tvs t) = case t of
--   (NTypeVariable t) -> if t `S.member` tvs then

pattern Sempty <- (S.null -> True) where Sempty = S.empty

occursIn :: Free x => TypeVariable -> x -> Bool
occursIn t x = t `S.member` free x

type NTypeS = Fix (Compose SchemeX NTypeF)

data SchemeX x = Scheme (Set TypeVariable) x
  deriving stock (Eq, Ord, Show)

instance Show1 SchemeX where
  liftShowsPrec s _ p (Scheme Sempty x) = s p x
  liftShowsPrec s _ p (Scheme tvs x) = ((("forall " <> show tvs)) <>) <> s p x

instance Eq1 SchemeX where
  liftEq eq (Scheme ts1 x1) (Scheme ts2 x2) = ts1 == ts2 && eq x1 x2

-- type SchemeS = SchemeX NTypeS

data Errors
  = UndefinedVariable VarName
  | UnexpectedType {expected :: NType, got :: NType}
  | KeyNotPresent Text

data Fresh x where
  Fresh :: Fresh TypeVariable

data ResumableExceptions x

type Runner effect = forall effs a. Eff (effect ': effs) a -> Eff effs a

runFresh :: Runner Fresh
runFresh =
  evalState (TypeVariable 0)
    . reinterpret
      ( \Fresh -> do
          (TypeVariable old) <- get
          put (TypeVariable $ old + 1)
          return (TypeVariable old)
      )

freshSrc ::
  Members '[Fresh, Reader SrcSpan, Writer (TypeVariable, SrcSpan)] r =>
  Eff r TypeVariable
freshSrc = do
  v <- send Fresh
  src <- ask @SrcSpan
  tell (v, src)
  return v

data UnifyingError
  = InfinityType NTypeS
  | CanNotUnify (NTypeF NTypeS) (NTypeF NTypeS)
  deriving stock (Eq, Show)

type UnifyM x = forall r. Members '[Error UnifyingError] r => Eff r x

type InferEffs =
  '[ Fresh,
     Writer (TypeVariable, SrcSpan),
     Writer Constraint,
     Writer (SrcSpan, Errors),
     Reader VariableMap
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

type VariableMap = Map VarName NTypeS

normalType :: NTypeF NTypeS -> NTypeS
normalType = Fix . Compose . Scheme S.empty

atomicType :: AtomicType -> NTypeS
atomicType = Fix . Compose . Scheme S.empty . NAtomic

tv :: TypeVariable -> NTypeS
tv = Fix . Compose . Scheme S.empty . NTypeVariable

(~~) :: Member (Writer Constraint) r => NTypeS -> NTypeS -> Eff r ()
lhs ~~ rhs = tell $ lhs :~ rhs

withBindings ::
  (Member (Reader r) effs, Semigroup r) =>
  r ->
  Eff effs a ->
  Eff effs a
withBindings bindings = local (bindings <>)

infer :: NExprLoc -> InferM NTypeS
infer (Fix (Compose (Ann src x))) = runReader src $ case x of
  NConstant a -> return . normalType $
    NAtomic $ case a of
      NURI {} -> URI
      NInt {} -> Integer
      NFloat {} -> Float
      NBool {} -> Bool
      NNull {} -> Null
  NLiteralPath {} -> return . normalType . NAtomic $ Path
  NEnvPath {} -> return . normalType . NAtomic $ Path
  NStr {} -> return . normalType $ NAtomic String
  NSym var ->
    ask @VariableMap
      >>= maybe (tv <$> throwSrcTV (UndefinedVariable var)) return . M.lookup var
  NList xs -> do
    xs' <- traverse infer xs
    traverse_ tell . fmap (uncurry (:~)) $ zip xs' (tail xs')
    case xs' of
      [] -> tv <$> freshSrc
      y : _ -> return . normalType $ List y
  NUnary NNeg y -> do
    t <- infer y
    -- TODO: Infer either Float or Integer
    -- tell $ t :~ (normalType . NAtomic $ )
    return t
  NUnary NNot y -> do
    t <- infer y
    t ~~ atomicType Bool
    return t
  NBinary NUpdate lhs rhs -> do
    lhsT <- infer lhs
    rhsT <- infer rhs
    lhsT ~~ rhsT
    lhs' <- expectAttrSet lhsT
    rhs' <- expectAttrSet rhsT
    forM_ (M.toList $ M.intersectionWith (,) lhs' rhs') (\(_, (a, b)) -> a ~~ b)
    return . normalType . NAttrSet $ rhs' <> lhs'
  NBinary NApp lhs rhs -> do
    lhst <- infer lhs
    rhst <- infer rhs
    rest <- tv <$> freshSrc
    lhst ~~ (normalType $ rhst :-> rest)
    return rhst
  NBinary NConcat lhs rhs -> do
    lhst <- infer lhs
    rhst <- infer rhs
    t <- tv <$> freshSrc
    let listt = normalType (List t)
    lhst ~~ listt
    rhst ~~ listt
    return $ listt
  NBinary op lhs rhs | op `elem` [NEq, NNEq] -> do
    lhst <- infer lhs
    rhst <- infer rhs
    lhst ~~ rhst
    return $ atomicType Bool
  NBinary op lhs rhs
    | op `elem` [NLt, NLte, NGt, NGte, NPlus, NMinus, NMult, NDiv] -> do
      lhst <- infer lhs
      rhst <- infer rhs
      -- TODO: Infer lhst and rhst are either Integer or Double
      lhst ~~ rhst
      return $ atomicType Bool
  NBinary op lhs rhs | op `elem` [NAnd, NOr, NImpl] -> do
    lhst <- infer lhs
    rhst <- infer rhs
    rhst ~~ atomicType Bool
    lhst ~~ atomicType Bool
    return $ atomicType Bool
  NSelect r path' def -> do
    setT <- infer r
    path <- getPath path'
    resT <- case path of
      Just p -> lookupAttrSet p setT
      Nothing -> tv <$> freshSrc
    case def of
      Nothing -> return ()
      Just defR -> do
        defT <- infer defR
        defT ~~ resT
    return resT
  NSet NNonRecursive xs -> normalType . NAttrSet <$> inferBinding xs
  NHasAttr attrSet path -> do
    setT <- infer attrSet
    expectAttrSet setT
    return setT
  NAbs param body -> do
    (paramT, varMap :: VariableMap) <- case param of
      Param name -> do
        t <- freshSrc
        return (tv t, M.singleton name $ tv t)
      ParamSet set variadic binding -> do
        setBindings <-
          M.fromList <$> set
            `for` ( \(name, def) -> do
                      t <- case def of
                        Nothing -> tv <$> freshSrc
                        Just def' -> infer def'
                      return (name, t)
                  )
        let setT = normalType . NAttrSet $ setBindings
        return (setT, maybe M.empty (`M.singleton` setT) binding <> setBindings)
    bodyT <- withBindings varMap $ infer body
    return . normalType $ paramT :-> bodyT
  NLet bindings body -> do
    bindingsT <- inferBinding bindings
    withBindings bindingsT $ infer body
  NIf cond t f -> do
    condT <- infer cond
    condT ~~ atomicType Bool
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
    condT ~~ atomicType Bool
    infer body
  NSynHole _ -> tv <$> freshSrc

inferBinding :: [Binding (Fix NExprLocF)] -> InferM' (Map VarName NTypeS)
inferBinding bindings = do
  bindings' <-
    bindings >>.= \case
      (NamedVar path r src) -> do
        getPath path >>= \case
          Nothing -> return []
          Just name -> do
            t <- infer r
            return $ [stack ((,) <$> name) (normalType . NAttrSet . uncurry M.singleton) t]
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

expectAttrSet :: Members '[Reader SrcSpan, Writer (SrcSpan, Errors)] r => NTypeS -> Eff r VariableMap
expectAttrSet (Fix (Compose (Scheme _ (NAttrSet x)))) = return x
expectAttrSet (Fix (Compose (Scheme _ g))) = do
  throwSrc
    UnexpectedType
      { expected = Fix $ NAttrSet M.empty,
        got = Fix $ const (Fix $ NAtomic Null) <$> g
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

mergeSetTypes :: NTypeS -> NTypeS -> NTypeS
mergeSetTypes (Fix (Compose (Scheme ts (NAttrSet ks)))) (Fix (Compose (Scheme ts' (NAttrSet ks')))) =
  Fix . Compose . Scheme (ts' <> ts) . NAttrSet $ M.unionWith mergeSetTypes ks ks'
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
  NTypeS ->
  Eff r NTypeS
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
      NTypeS ->
      Eff r NTypeS
    go [] x = return x
    go (p : ps) x = do
      expectAttrSet x <&> M.lookup p >>= \case
        Nothing -> tv <$> throwSrcTV (KeyNotPresent p)
        Just t -> go ps t
