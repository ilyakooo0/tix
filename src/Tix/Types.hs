module Tix.Types
  ( NType (..),
    FieldStrictness (..),
    AtomicType (..),
    DeBruijn (..),
    TypeVariable (..),
    RangeTypeVariable (..),
    rangeTypeVariable,
    Scheme (..),
    scheme,
    Pred (..),
    (//),
    TraversableNTypes (..),
    traverseNTypes,
    TerminalNType (..),
  )
where

import Control.Lens hiding (List)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.MultiKey.Strict as MKM
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Generic.Data
import Prettyprinter

data Pred
  = -- | '(x // y) ~ z'
    Update
      !NType
      -- ^ x
      !NType
      -- ^ y
      !NType
      -- ^ z
  | HasField !FieldStrictness !NType !(Text, Scheme)
  | -- | supports '+' operator (strings and numbers)
    StringOrNumber !NType
  deriving stock (Eq, Ord, Show, Generic)

data FieldStrictness = OptionalField | RequiredField
  deriving stock (Eq, Ord, Show, Generic)

instance MKM.Keyable Pred where
  type Key Pred = TypeVariable
  getKeys p =
    S.toList $
      p
        ^. traverseNTypes
          . to
            ( \case
                TAtomic _ -> S.empty
                TTypeVariable t -> S.singleton t
                TBruijn _ -> S.empty
            )

(//) :: NType -> NType -> NType -> Pred
(//) = Update
{-# INLINE (//) #-}

data Scheme = [Pred] :=> !NType
  deriving stock (Eq, Ord, Show, Generic)

scheme :: NType -> Scheme
scheme t = [] :=> t
{-# INLINE scheme #-}

data TerminalNType
  = TAtomic !AtomicType
  | TTypeVariable !TypeVariable
  | TBruijn !DeBruijn
  deriving stock (Show, Generic)

class TraversableNTypes s where
  traverseNTypesWith ::
    (Applicative f) => (forall x. f x -> f x) -> (TerminalNType -> f TerminalNType) -> s -> f s

instance TraversableNTypes NType where
  traverseNTypesWith ctx f = \case
    NAtomic a -> f' $ TAtomic a
    NTypeVariable a -> f' $ TTypeVariable a
    NBruijn a -> f' $ TBruijn a
    x :-> y -> ctx $ (:->) <$> traverseNextType x <*> traverseNextType y
    List t -> ctx $ List <$> traverseNextType t
    NAttrSet m -> ctx $ NAttrSet <$> traverseNTypesWith ctx f `traverse` m
    where
      traverseNextType = traverseNTypesWith ctx f
      f' = fmap fromTerminalNType . f
      fromTerminalNType :: TerminalNType -> NType
      fromTerminalNType = \case
        TAtomic a -> NAtomic a
        TTypeVariable a -> NTypeVariable a
        TBruijn a -> NBruijn a
  {-# INLINE traverseNTypesWith #-}

instance TraversableNTypes Scheme where
  traverseNTypesWith ctx f (cs :=> t) =
    ctx $
      (:=>) <$> traverseNTypesWith ctx f `traverse` cs <*> traverseNTypesWith ctx f t
  {-# INLINE traverseNTypesWith #-}

instance TraversableNTypes Pred where
  traverseNTypesWith ctx f = \case
    (HasField r t (k, h)) ->
      HasField r <$> traverseNTypesWith ctx f t <*> ((k,) <$> traverseNTypesWith ctx f h)
    (Update x y z) ->
      Update
        <$> traverseNTypesWith ctx f x
        <*> traverseNTypesWith ctx f y
        <*> traverseNTypesWith ctx f z
    (StringOrNumber x) -> StringOrNumber <$> traverseNTypesWith ctx f x
  {-# INLINE traverseNTypesWith #-}

traverseNTypes :: TraversableNTypes s => Traversal' s TerminalNType
traverseNTypes = traverseNTypesWith id
{-# INLINE traverseNTypes #-}

data NType
  = NTypeVariable !TypeVariable
  | NBruijn !DeBruijn
  | NAtomic !AtomicType
  | !NType :-> !NType
  | List !NType
  | NAttrSet !(Map Text Scheme)
  deriving stock (Eq, Ord, Show, Generic)

data AtomicType
  = Number
  | Bool
  | String
  | Path
  | Null
  | URI
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty AtomicType where
  pretty = viaShow

data DeBruijn = DeBruijn !Int !Int
  deriving stock (Show, Eq, Ord)

data TypeVariable
  = TypeVariable !Int
  | SubTypeVariable !Int !Int
  deriving stock (Eq, Ord)

rangeTypeVariable :: TypeVariable -> RangeTypeVariable
rangeTypeVariable (TypeVariable x) = RangeTypeVariable x
rangeTypeVariable (SubTypeVariable x _) = RangeTypeVariable x
{-# INLINE rangeTypeVariable #-}

newtype RangeTypeVariable = RangeTypeVariable {unRangeTypeVariable :: Int}
  deriving newtype (Eq, Ord, Enum, Bounded)

instance Show RangeTypeVariable where
  show (RangeTypeVariable n) = "〚" <> show n <> "〛"

instance Show TypeVariable where
  show = show . pretty

instance Pretty TypeVariable where
  pretty (TypeVariable n) = "〚" <> pretty n <> "〛"
  pretty (SubTypeVariable tv n) = "〚" <> pretty tv <> "." <> pretty n <> "〛"

instance Pretty DeBruijn where
  pretty (DeBruijn i j) = "〚" <> pretty i <+> pretty j <> "〛"

instance Pretty NType where
  pretty t = greekifyWith t $ prettyNType False t

instance Pretty Scheme where
  pretty t = greekifyWith t $ prettyScheme t

greekifyWith :: TraversableNTypes t => t -> Eff '[Reader (Map DeBruijn Text), Reader Depth] x -> x
greekifyWith t =
  run
    . runReader (Depth 0)
    . runReader (M.fromList $ zip (S.toList $ getAllDeBurjins t) variableNames)

newtype Depth = Depth Int

descend :: Member (Reader Depth) eff => Eff eff x -> Eff eff x
descend = local (\(Depth n) -> Depth $ n + 1)

getDepthedDeBurjins :: (TraversableNTypes x, Member (Reader Depth) eff) => x -> Eff eff (Set DeBruijn)
getDepthedDeBurjins x = do
  Depth depth <- ask
  return $ S.mapMonotonic (DeBruijn (- depth)) $ getDeBurjins x

getDeBurjin :: (Members '[Reader Depth, Reader (Map DeBruijn Text)] eff) => DeBruijn -> Eff eff (Maybe Text)
getDeBurjin (DeBruijn i j) = do
  Depth depth <- ask
  M.lookup (DeBruijn (i - depth) j) <$> ask

type PrettyM a = forall eff. Members '[Reader Depth, Reader (Map DeBruijn Text)] eff => Eff eff a

prettyNType :: Bool -> NType -> PrettyM (Doc ann)
prettyNType _ (NTypeVariable x) = return $ pretty x
prettyNType _ (NBruijn x) =
  getDeBurjin x <&> \case
    Nothing -> pretty x
    Just t -> pretty t
prettyNType _ (NAtomic x) = return $ pretty x
prettyNType rhs (x :-> y) = descend $ do
  x' <- prettyNType True x
  y' <- prettyNType False y
  return $ group $ (if rhs then bracketed "(" ")" else id) $ x' <+> "->" <> line <> y'
prettyNType _ (List x) = descend $ do
  x' <- prettyNType True x
  return $ group $ bracketed "[" "]" x'
prettyNType _ (NAttrSet m) = descend $ do
  decls <- prettyDecl `traverse` M.toAscList m
  return $ group $ bracketed "{" "}" $ vsep decls
  where
    prettyDecl :: (Text, Scheme) -> PrettyM (Doc ann)
    prettyDecl (name, sch) = do
      sch' <- prettyScheme sch
      return $ group $ pretty name <+> "=" <> nest 2 (line <> sch' <> ";")

bracketed :: Doc ann -> Doc ann -> Doc ann -> Doc ann
bracketed l r d = flatAlt (l <> " ") l <> nest 2 d <> line' <> r

prettyScheme :: Scheme -> PrettyM (Doc ann)
prettyScheme u@(cs :=> t) = do
  greekVars <- ask @(Map DeBruijn Text)
  foralls <-
    getDepthedDeBurjins u <&> \s -> case M.elems . M.restrictKeys greekVars $ s of
      [] -> mempty
      foralls -> "∀" <+> fillSep (pretty <$> foralls) <> "." <> line
  descend $ do
    cs' <-
      prettyPred `traverse` cs <&> \case
        [] -> mempty
        cs' -> group (paren (vsep $ punctuate "," cs') <+> "=>" <> line)
    t' <- prettyNType False t
    return $ group $ foralls <> cs' <> t'
  where
    prettyPred :: Pred -> PrettyM (Doc ann)
    prettyPred (HasField r h (k, v)) = do
      h' <- prettyNType False h
      v' <- prettyScheme v
      let dotOp = case r of
            OptionalField -> ".?"
            RequiredField -> "."
      return $ group $ group (h' <> nest 2 (line' <> dotOp <> pretty k <+> "=")) <> nest 2 (line <> v')
    prettyPred (Update x y z) = do
      x' <- prettyNType False x
      y' <- prettyNType False y
      z' <- prettyNType False z
      return $ group $ paren x' <+> "//" <> line <> paren y' <+> "~" <> line <> paren z'
    prettyPred (StringOrNumber x) = do
      x' <- prettyNType False x
      return $ "(String | Number)" <+> "~" <+> x'
    paren = bracketed "(" ")"

-- | Returns the set of all De Bruijn type variables relative to the outermost
-- (current) binding context.
-- So variables that are bound in one of the inner contexts will have a negative
-- binding context number.
getAllDeBurjins :: TraversableNTypes x => x -> Set DeBruijn
getAllDeBurjins =
  view $
    traverseNTypesWith (first $ S.mapMonotonic (\(DeBruijn i j) -> DeBruijn (i - 1) j))
      . to
        ( \case
            TAtomic _ -> S.empty
            TBruijn b -> S.singleton b
            TTypeVariable _ -> S.empty
        )

-- | Get all De Bruijn type variables that were bound in the the outermost
-- (current) binding context.
-- Only returns the second indexes.
getDeBurjins :: TraversableNTypes x => x -> Set Int
getDeBurjins t = getDeBurjins' t 0

-- | Gets all De Bruijn variables with the given binding context offset.
-- Only returns the second indexes.
getDeBurjins' :: TraversableNTypes x => x -> Int -> Set Int
getDeBurjins' =
  view $
    traverseNTypesWith (\(Const f) -> Const (\n -> f (n + 1)))
      . to
        ( \case
            TAtomic _ -> const S.empty
            TBruijn (DeBruijn m j) -> \n ->
              if m == n
                then S.singleton j
                else S.empty
            TTypeVariable _ -> const S.empty
        )

variableNames :: [Text]
variableNames =
  fmap T.pack $
    greek <> foldMap (\n -> (<> show n) <$> greek) ([1 :: Int ..] :: [Int])
  where
    greek :: [String]
    greek = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]
