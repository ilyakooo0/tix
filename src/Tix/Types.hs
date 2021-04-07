module Tix.Types
  ( NType (..),
    AtomicType (..),
    DeBruijn (..),
    TypeVariable (..),
    Scheme (..),
    scheme,
    Pred (..),
    (//),
    Preds (..),
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
import GHC.Exts
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
  | !NType `HasField` !(Text, Scheme)
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

-- this seems bad. It should not drop constraints.
(//) :: NType -> NType -> NType -> Pred
(//) = Update
{-# INLINE (//) #-}

data Scheme = Preds :=> !NType
  deriving stock (Eq, Ord, Show, Generic)

newtype Preds = Preds {unPreds :: [Pred]}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid, IsList)
  deriving stock (Generic)

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
      (:=>) <$> traverseNTypesWith ctx f cs <*> traverseNTypesWith ctx f t
  {-# INLINE traverseNTypesWith #-}

instance TraversableNTypes Preds where
  traverseNTypesWith ctx f (Preds ps) = Preds <$> traverseNTypesWith ctx f `traverse` ps
  {-# INLINE traverseNTypesWith #-}

instance TraversableNTypes Pred where
  traverseNTypesWith ctx f = \case
    (HasField t (k, h)) ->
      HasField <$> traverseNTypesWith ctx f t <*> ((k,) <$> traverseNTypesWith ctx f h)
    (Update x y z) ->
      Update
        <$> traverseNTypesWith ctx f x
        <*> traverseNTypesWith ctx f y
        <*> traverseNTypesWith ctx f z
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
  = Integer
  | Float
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

newtype TypeVariable = TypeVariable Int
  deriving newtype (Eq, Ord, Enum, Bounded)

instance Show TypeVariable where
  show (TypeVariable n) = "〚" <> show n <> "〛"

instance Pretty TypeVariable where
  pretty (TypeVariable n) = "〚" <> pretty n <> "〛"

instance Pretty DeBruijn where
  pretty (DeBruijn i j) = "〚" <> pretty i <+> pretty j <> "〛"

instance Pretty NType where
  pretty t = greekifyWith t $ prettyNType t

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

prettyNType :: NType -> PrettyM (Doc ann)
prettyNType (NTypeVariable x) = return $ pretty x
prettyNType (NBruijn x) =
  getDeBurjin x <&> \case
    Nothing -> pretty x
    Just t -> pretty t
prettyNType (NAtomic x) = return $ pretty x
prettyNType (x :-> y) = descend $ do
  x' <- prettyNType x
  y' <- prettyNType y
  return $ group $ x' <+> "->" <> line <> y'
prettyNType (List x) = descend $ do
  x' <- prettyNType x
  return $ group $ bracketed "[" "]" x'
prettyNType (NAttrSet m) = descend $ do
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
prettyScheme u@(Preds cs :=> t) = do
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
    t' <- prettyNType t
    return $ group $ foralls <> cs' <> t'
  where
    prettyPred :: Pred -> PrettyM (Doc ann)
    prettyPred (h `HasField` (k, v)) = do
      h' <- prettyNType h
      v' <- prettyScheme v
      return $ group $ group (h' <> nest 2 (line' <> "." <> pretty k <+> "=")) <> nest 2 (line <> v')
    prettyPred (Update x y z) = do
      x' <- prettyNType x
      y' <- prettyNType y
      z' <- prettyNType z
      return $ group $ paren x' <+> "//" <> line <> paren y' <+> "~" <> line <> paren z'
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

greek :: [Text]
greek = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]

variableNames :: [Text]
variableNames =
  greek <> fmap (\(n, g) -> g <> T.pack (show n)) (zip [1 :: Int ..] greek)
