module Tix.Typechecker
  ( infer,
    solve,
    runFresh,
    runSeqWriter,
    getType,
    showNType,
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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T
import Data.Traversable
import Debug.Trace
import Nix.Atoms
import Nix.Expr

getType :: NExprLoc -> (NType, [(SrcSpan, Errors)], [UnifyingError])
getType r =
  let ((((t, unifying), srcs), constraints), errs) =
        run
          . runReader (M.empty :: VariableMap)
          . runSeqWriter @[(SrcSpan, Errors)]
          . runSeqWriter @(Seq Constraint)
          . runSeqWriter @[(TypeVariable, SrcSpan)]
          . runSeqWriter @[UnifyingError]
          . runFresh
          $ getType' r
   in (t, errs, unifying)

getType' ::
  Members
    '[ Writer (SrcSpan, Errors),
       Reader VariableMap,
       Writer (TypeVariable, SrcSpan),
       Writer UnifyingError,
       Fresh
     ]
    r =>
  NExprLoc ->
  Eff r NType
getType' r = do
  (t, constraints) <-
    runSeqWriter @(Seq Constraint) $
      infer r
  s <- solve (traceShowId constraints)
  return . close $ sub s (traceShowId t)

newtype TypeVariable = TypeVariable Int
  deriving newtype (Show, Eq, Ord)

data Constraint = !NType :~ !NType
  deriving stock (Eq, Ord, Show)

data DeBrujin = DeBrujin !Int !Int
  deriving stock (Show, Eq, Ord)

greek :: [T.Builder]
greek = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]

variableNames :: [T.Builder]
variableNames =
  greek
    <> (fmap (\(n, g) -> g <> T.fromString (show n)) $ zip [1 :: Int ..] greek)

showNType :: NType -> Text
showNType t = TL.toStrict . T.toLazyText $ showNType' t
  where
    greekVars = M.fromList $ zip (S.toList $ getDeBurjins t) variableNames
    showNType' :: NType -> T.Builder
    showNType' (NAtomic a) = showAtomicType a
    showNType' (NBrujin b) = greekVars M.! b
    showNType' (NTypeVariable _) = error "should not have free type variables at this point"
    showNType' (x :-> y) = showNType' x <> " -> " <> showNType' y
    showNType' (List x) = "[" <> showNType' x <> "]"
    showNType' (NAttrSet x) =
      "AttrSet {"
        <> ( mconcat
               . intersperse "; "
               . fmap (\(k, v) -> T.fromText k <> " :: " <> showNType' v)
               $ M.toList x
           )
        <> "}"

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
  | !NType :-> !NType
  | -- These two introduce a new De Brujin binding context
    List !NType
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

getDeBurjins :: NType -> Set DeBrujin
getDeBurjins (NTypeVariable _) = S.empty
getDeBurjins (NBrujin x) = S.singleton x
getDeBurjins (NAtomic _) = S.empty
getDeBurjins (x :-> y) = getDeBurjins x <> getDeBurjins y
getDeBurjins (List x) = getDeBurjins x
getDeBurjins (NAttrSet xs) = foldMap getDeBurjins . M.elems $ xs

isClosed :: NType -> Bool
isClosed (NTypeVariable _) = False
isClosed (NBrujin _) = True
isClosed (NAtomic _) = True
isClosed (x :-> y) = isClosed x && isClosed y
isClosed (List x) = isClosed x
isClosed (NAttrSet xs) = all isClosed . M.elems $ xs

close :: NType -> NType
close =
  run
    . evalState @(Map TypeVariable DeBrujin) M.empty
    . runReader @Int 0
    . evalState @Int 0
    . close'
  where
    -- State is the sequential number, Reader is the binding context number
    close' :: NType -> Eff '[State Int, Reader Int, State (Map TypeVariable DeBrujin)] NType
    close' x@(NAtomic _) = return x
    close' x@(NBrujin _) = return x
    close' (x :-> y) = (:->) <$> close' x <*> close' y
    close' (NTypeVariable tv) = NBrujin <$> newbrujin tv
    close' (List x) = bndCtx $ List <$> close' x
    close' (NAttrSet x) = bndCtx $ NAttrSet <$> traverse close' x

    bndCtx = local @Int (+ 1)
    newbrujin t = do
      get @(Map TypeVariable DeBrujin) <&> M.lookup t >>= \case
        Nothing -> do
          i <- ask
          j <- get
          modify @Int (+ 1)
          let x = DeBrujin i j
          modify @(Map TypeVariable DeBrujin) (M.insert t x)
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

newtype Substitution = Substitution {unSubstitution :: Map TypeVariable NType}

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
unify (lhs :~ rhs) =
  case (lhs, rhs) of
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
    (x, y) -> tell (CanNotUnify x y) >> return mempty

solve :: Seq Constraint -> UnifyM Substitution
solve Empty = return mempty
solve (rest :|> c) = do
  s <- unify c
  (<> s) <$> solve (fmap (subCon (traceShowId s)) rest)

pattern Sempty <- (S.null -> True) where Sempty = S.empty

occursIn :: Free x => TypeVariable -> x -> Bool
occursIn t x = t `S.member` free x

data SchemeX x = Scheme (Set TypeVariable) x
  deriving stock (Eq, Ord, Show)

instance Show1 SchemeX where
  liftShowsPrec s _ p (Scheme Sempty x) = s p x
  liftShowsPrec s _ p (Scheme tvs x) = ((("forall " <> show tvs)) <>) <> s p x

instance Eq1 SchemeX where
  liftEq eq (Scheme ts1 x1) (Scheme ts2 x2) = ts1 == ts2 && eq x1 x2

-- type SchemeS = SchemeX NType

data Errors
  = UndefinedVariable VarName
  | UnexpectedType {expected :: NType, got :: NType}
  | KeyNotPresent Text
  deriving stock (Eq, Ord, Show)

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
     Writer UnifyingError
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
    | op `elem` [NLt, NLte, NGt, NGte, NPlus, NMinus, NMult, NDiv] -> do
      lhst <- infer lhs
      rhst <- infer rhs
      -- TODO: Infer lhst and rhst are either Integer or Double
      lhst ~~ rhst
      return $ NAtomic Bool
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
            t <- infer r
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
