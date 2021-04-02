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
    foldNTypeM,
    foldNType,
    NTypeFolder (..),
  )
where

import Control.Applicative
import Data.Foldable
import Data.Functor.Identity
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T
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
  deriving stock (Eq, Ord, Show)

-- this seems bad. It should not drop constraints.
(//) :: Scheme -> Scheme -> Scheme -> Pred
(//) (_ :=> x) (_ :=> y) (_ :=> z) = Update x y z

data Scheme = Preds :=> !NType
  deriving stock (Eq, Ord, Show)

newtype Preds = Preds {unPreds :: [Pred]}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid, IsList)

(=>>) :: Preds -> Scheme -> Scheme
[] =>> x = x
xs =>> (ys :=> u) = (xs <> ys) :=> u

scheme :: NType -> Scheme
scheme t = [] :=> t

data NTypeFolder a = NTypeFolder
  { variableFolder :: TypeVariable -> a,
    deBruijnFolder :: DeBruijn -> a,
    atomicFolder :: AtomicType -> a,
    compositeFolder :: a -> a
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically (NTypeFolder a)

class TypeFoldable t where
  foldNTypeM :: (Applicative m, Monoid a) => NTypeFolder (m a) -> t -> m a

instance TypeFoldable Scheme where
  foldNTypeM tf (Preds ps :=> t) = foldA [foldAType tf ps, foldNTypeM tf t]

instance TypeFoldable a => TypeFoldable [a] where
  foldNTypeM tf = foldAType tf

instance TypeFoldable Pred where
  foldNTypeM tf (Update x y z) = foldAType tf [x, y, z]
  foldNTypeM tf (HasField x (_, z)) = foldA [foldNTypeM tf x, foldNTypeM tf z]

instance TypeFoldable NType where
  foldNTypeM NTypeFolder {variableFolder = f} (NTypeVariable x) = f x
  foldNTypeM NTypeFolder {deBruijnFolder = f} (NBruijn x) = f x
  foldNTypeM NTypeFolder {atomicFolder = f} (NAtomic x) = f x
  foldNTypeM tf@NTypeFolder {compositeFolder = f} (x :-> y) = f $ foldAType tf [x, y]
  foldNTypeM tf@NTypeFolder {compositeFolder = f} (List x) = f $ foldNTypeM tf x
  foldNTypeM tf@NTypeFolder {compositeFolder = f} (NAttrSet x) = f $ foldAType tf $ M.elems x

foldNType :: (Monoid a, TypeFoldable x) => NTypeFolder a -> x -> a
foldNType NTypeFolder {..} = runIdentity . foldNTypeM tf
  where
    tf =
      NTypeFolder
        { variableFolder = Identity . variableFolder,
          deBruijnFolder = Identity . deBruijnFolder,
          atomicFolder = Identity . atomicFolder,
          compositeFolder = Identity . compositeFolder . runIdentity
        }

foldAType :: (Monoid a, Applicative f, TypeFoldable t) => NTypeFolder (f a) -> [t] -> f a
foldAType tf ne = foldA $ fmap (foldNTypeM tf) ne

foldA :: (Monoid a, Applicative f) => [f a] -> f a
foldA = foldr (liftA2 (<>)) (pure mempty)

data NType
  = NTypeVariable !TypeVariable
  | NBruijn !DeBruijn
  | NAtomic !AtomicType
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
    showNType' depth (Preds ps :=> u) =
      foralls <> predicates <> case u of
        (NAtomic a) -> showAtomicType a
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
getAllDeBurjins :: TypeFoldable x => x -> Set DeBruijn
getAllDeBurjins =
  foldNType
    NTypeFolder
      { variableFolder = const S.empty,
        deBruijnFolder = S.singleton,
        atomicFolder = const S.empty,
        compositeFolder = S.mapMonotonic (\(DeBruijn i j) -> DeBruijn (i - 1) j)
      }

-- | Get all De Bruijn type variables that were bound in the the outermost
-- (current) binding context.
-- Only returns the second indexes.
getDeBurjins :: NType -> Set Int
getDeBurjins t = getDeBurjins' t 0

-- | Gets all De Bruijn variables with the given binding context offset.
-- Only returns the second indexes.
getDeBurjins' :: TypeFoldable x => x -> Int -> Set Int
getDeBurjins' =
  foldNType
    NTypeFolder
      { variableFolder = \_ _ -> S.empty,
        deBruijnFolder = \t n -> case t of
          DeBruijn m j | m == n -> S.singleton j
          _ -> S.empty,
        atomicFolder = \_ _ -> S.empty,
        compositeFolder = \f n -> f (n + 1)
      }

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
  pretty (NAtomic a) = viaShow a
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
  pretty ([] :=> t) = pretty t
  pretty (cs :=> t) = viaShow cs <+> "=>" <+> pretty t
