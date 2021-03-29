module Tix.Types
  ( NType (..),
    AtomicType (..),
    DeBruijn (..),
    TypeVariable (..),
    showNType,
    Scheme (..),
    scheme,
    Pred (..),
    (=>>),
    (//),
    Preds (..),
  )
where

import Data.Foldable
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T
import GHC.Exts
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
  deriving stock (Eq, Ord, Show)

(//) :: Scheme -> Scheme -> Scheme -> Pred
(//) (_ :=> x) (_ :=> y) (_ :=> z) = Update x y z
(//) _ _ _ = error "Bad update operator" -- This should not be an 'error'

data Scheme
  = Preds :=> !NType
  | NAtomic !AtomicType
  deriving stock (Eq, Ord, Show)

newtype Preds = Preds {unPreds :: [Pred]}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid, IsList)

(=>>) :: Preds -> Scheme -> Scheme
[] =>> x = x
xs =>> (ys :=> u) = (xs <> ys) :=> u
_ =>> (NAtomic _) = error "Can not constrain atomic type"

scheme :: NType -> Scheme
scheme t = [] :=> t

data NType
  = NTypeVariable !TypeVariable
  | NBruijn !DeBruijn
  | !Scheme :-> !Scheme
  | List !Scheme
  | NAttrSet !(Map Text Scheme)
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

data DeBruijn = DeBruijn !Int !Int
  deriving stock (Show, Eq, Ord)

newtype TypeVariable = TypeVariable Int
  deriving newtype (Eq, Ord, Enum, Bounded)

instance Show TypeVariable where
  show (TypeVariable n) = "〚" <> show n <> "〛"

-- | Show a type which has already been closed over.
--
-- TODO: This should be a Prettyprinter
showNType :: Scheme -> Text
showNType t = TL.toStrict . T.toLazyText $ showNType' 0 t
  where
    greekVars = M.fromList $ zip (S.toList $ getAllDeBurjins t) variableNames
    showNType' :: Int -> Scheme -> T.Builder
    showNType' _ (NAtomic a) = showAtomicType a
    showNType' depth (Preds ps :=> u) =
      foralls <> predicates <> case u of
        (NBruijn (DeBruijn i j)) -> greekVars M.! DeBruijn (i - depth) j
        (NTypeVariable _) -> error "should not have free type variables at this point"
        (x :-> y) -> showNextNType' x <> " -> " <> showNextNType' y
        (List x) -> "[" <> showNextNType' x <> "]"
        (NAttrSet x) ->
          "{ "
            <> ( mconcat
                   . L.intersperse "; "
                   . fmap (\(k, v) -> T.fromText k <> " = " <> showNextNType' v)
                   $ M.toList x
               )
            <> " }"
      where
        showNextNType' = showNType' $ depth + 1
        currBurjins = S.mapMonotonic (DeBruijn (- depth)) $ getDeBurjins u
        foralls =
          if S.null currBurjins
            then ""
            else
              "∀ "
                <> (mconcat . L.intersperse " " . M.elems $ M.restrictKeys greekVars currBurjins)
                <> ". "
        predicates =
          if null ps
            then mempty
            else "(" <> (fold . L.intersperse ", " $ T.fromString . show <$> ps) <> ") => "

-- | Returns the set of all De Bruijn type variables relative to the outermost
-- (current) binding context.
-- So variables that are bound in one of the inner contexts will have a negative
-- binding context number.
getAllDeBurjins :: Scheme -> Set DeBruijn
getAllDeBurjins (NAtomic _) = S.empty
getAllDeBurjins (Preds ps :=> t) =
  foldMap
    ( \case
        (Update x y z) ->
          bndCtx $
            getAllTypeDeBurjins x <> getAllTypeDeBurjins y <> getAllTypeDeBurjins z
        (HasField x (_, y)) -> bndCtx $ getAllTypeDeBurjins x <> getAllDeBurjins y
    )
    ps
    <> case t of
      (NTypeVariable _) -> S.empty
      (NBruijn x) -> S.singleton x
      (x :-> y) -> bndCtx $ getAllDeBurjins x <> getAllDeBurjins y
      (List x) -> bndCtx $ getAllDeBurjins x
      (NAttrSet xs) -> bndCtx . foldMap getAllDeBurjins . M.elems $ xs
  where
    bndCtx = S.mapMonotonic (\(DeBruijn i j) -> DeBruijn (i - 1) j)

getAllTypeDeBurjins :: NType -> Set DeBruijn
getAllTypeDeBurjins = getAllDeBurjins . scheme

-- | Get all De Bruijn type variables that were bound in the the outermost
-- (current) binding context.
-- Only returns the second indexes.
getDeBurjins :: NType -> Set Int
getDeBurjins = getDeBurjins' 0

-- | Gets all De Bruijn variables with the given binding context offset.
-- Only returns the second indexes.
getDeBurjins' :: Int -> NType -> Set Int
getDeBurjins' _ (NTypeVariable _) = S.empty
getDeBurjins' n (NBruijn (DeBruijn m j)) | m == n = S.singleton j
getDeBurjins' _ (NBruijn _) = S.empty
getDeBurjins' n (x :-> y) = getSchemeDeBurjins' (n + 1) x <> getSchemeDeBurjins' (n + 1) y
getDeBurjins' n (List x) = getSchemeDeBurjins' (n + 1) x
getDeBurjins' n (NAttrSet xs) = foldMap (getSchemeDeBurjins' (n + 1)) . M.elems $ xs

getSchemeDeBurjins' :: Int -> Scheme -> Set Int
getSchemeDeBurjins' _ (NAtomic _) = S.empty
getSchemeDeBurjins' n (Preds ps :=> t) =
  getDeBurjins' n t
    <> foldMap
      ( \case
          Update x y z -> getDeBurjins' n x <> getDeBurjins' n y <> getDeBurjins' n z
          HasField u (_, v) -> getDeBurjins' n u <> getSchemeDeBurjins' n v
      )
      ps

showAtomicType :: AtomicType -> T.Builder
showAtomicType Integer = "Integer"
showAtomicType Float = "Float"
showAtomicType Bool = "Bool"
showAtomicType String = "String"
showAtomicType Path = "Path"
showAtomicType Null = "Null"
showAtomicType URI = "URI"

greek :: [T.Builder]
greek = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]

variableNames :: [T.Builder]
variableNames =
  greek <> fmap (\(n, g) -> g <> T.fromString (show n)) (zip [1 :: Int ..] greek)

instance Pretty NType where
  pretty (NTypeVariable t) = viaShow t
  pretty (NBruijn (DeBruijn i j)) = "⟦" <> pretty i <+> pretty j <> "⟧"
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

instance Pretty Scheme where
  pretty (NAtomic a) = viaShow a
  pretty ([] :=> t) = pretty t
  pretty (cs :=> t) = viaShow cs <+> "=>" <+> pretty t
