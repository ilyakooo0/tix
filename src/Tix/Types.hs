module Tix.Types
  ( NType (..),
    AtomicType (..),
    DeBrujin (..),
    TypeVariable (..),
    showNType,
  )
where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T
import Prettyprinter

data NType
  = NTypeVariable !TypeVariable
  | NBrujin !DeBrujin
  | NAtomic !AtomicType
  | -- These three introduce a new De Brujin binding context
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

data DeBrujin = DeBrujin !Int !Int
  deriving stock (Show, Eq, Ord)

newtype TypeVariable = TypeVariable Int
  deriving newtype (Eq, Ord, Enum, Bounded)

instance Show TypeVariable where
  show (TypeVariable n) = "〚" <> show n <> "〛"

-- | Show a type which has already been closed over.
--
-- TODO: This should be a Prettyprinter
showNType :: NType -> Text
showNType t = TL.toStrict . T.toLazyText $ showNType' 0 t
  where
    greekVars = M.fromList $ zip (S.toList $ getAllDeBurjins t) variableNames
    showNType' :: Int -> NType -> T.Builder
    showNType' depth u =
      foralls <> case u of
        (NAtomic a) -> showAtomicType a
        (NBrujin (DeBrujin i j)) -> greekVars M.! DeBrujin (i - depth) j
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
        currBurjins = S.mapMonotonic (DeBrujin (- depth)) $ getDeBurjins u
        foralls =
          if S.null currBurjins
            then ""
            else
              "∀ "
                <> (mconcat . L.intersperse " " . M.elems $ M.restrictKeys greekVars currBurjins)
                <> ". "

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
