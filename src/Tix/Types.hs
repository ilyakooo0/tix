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
    traverseNTypesWith,
    traverseNTypes,
    TerminalNType (..),
  )
where

import Control.Lens hiding (List)
import Data.Bifunctor
import Data.Data (Data)
import Data.Data.Lens
import Data.Foldable
import Data.Generics.Sum.Subtype
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.MultiKey.Strict as MKM
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T
import GHC.Exts
import Generic.Data
import Prettyprinter
import Type.Reflection

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
  deriving stock (Eq, Ord, Show, Data, Generic)

instance MKM.Keyable Pred where
  type Key Pred = TypeVariable
  getKeys p = p ^.. template @_ @TypeVariable

-- this seems bad. It should not drop constraints.
(//) :: NType -> NType -> NType -> Pred
(//) = Update

data Scheme = Preds :=> !NType
  deriving stock (Eq, Ord, Show, Data, Generic)

newtype Preds = Preds {unPreds :: [Pred]}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid, IsList)
  deriving stock (Data, Generic)

(=>>) :: Preds -> Scheme -> Scheme
[] =>> x = x
xs =>> (ys :=> u) = (xs <> ys) :=> u

scheme :: NType -> Scheme
scheme t = [] :=> t

data TerminalNType
  = TAtomic !AtomicType
  | TTypeVariable !TypeVariable
  | TBruijn !DeBruijn
  deriving stock (Show, Generic)

traverseNTypesWith ::
  (Data s, Applicative f) => (forall x. f x -> f x) -> (TerminalNType -> f TerminalNType) -> s -> f s
traverseNTypesWith ctx f s = case eqTypeRep (typeOf s) (typeRep @NType) of
  Just HRefl | Just _ <- projectSub @TerminalNType s -> _Sub @TerminalNType f s
  Just HRefl -> ctx $ (template @_ @NType . traverseNTypesWith ctx) f s
  _ -> (template @_ @NType . traverseNTypesWith ctx) f s
{-# SPECIALIZE traverseNTypesWith ::
  Applicative f => (forall x. f x -> f x) -> (TerminalNType -> f TerminalNType) -> NType -> f NType
  #-}
{-# SPECIALIZE traverseNTypesWith ::
  Applicative f => (forall x. f x -> f x) -> (TerminalNType -> f TerminalNType) -> Scheme -> f Scheme
  #-}
{-# SPECIALIZE traverseNTypesWith ::
  Monoid r => (forall x. Const r x -> Const r x) -> (TerminalNType -> Const r TerminalNType) -> NType -> Const r NType
  #-}
{-# SPECIALIZE traverseNTypesWith ::
  Monoid r => (forall x. Const r x -> Const r x) -> (TerminalNType -> Const r TerminalNType) -> Scheme -> Const r Scheme
  #-}

traverseNTypes :: Data s => Traversal' s TerminalNType
traverseNTypes = traverseNTypesWith id
{-# INLINE traverseNTypes #-}

data NType
  = NTypeVariable !TypeVariable
  | NBruijn !DeBruijn
  | NAtomic !AtomicType
  | !NType :-> !NType
  | List !NType
  | NAttrSet !(Map Text Scheme)
  deriving stock (Eq, Ord, Show, Data, Generic)

data AtomicType
  = Integer
  | Float
  | Bool
  | String
  | Path
  | Null
  | URI
  deriving stock (Eq, Ord, Show, Data, Generic)

data DeBruijn = DeBruijn !Int !Int
  deriving stock (Show, Eq, Ord, Data)

newtype TypeVariable = TypeVariable Int
  deriving newtype (Eq, Ord, Enum, Bounded)
  deriving stock (Data)

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
        (x :-> y) -> showNextNType' (scheme x) <> " -> " <> showNextNType' (scheme y)
        (List x) -> "[" <> showNextNType' (scheme x) <> "]"
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
getAllDeBurjins :: Data x => x -> Set DeBruijn
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
getDeBurjins :: NType -> Set Int
getDeBurjins t = getDeBurjins' t 0

-- | Gets all De Bruijn variables with the given binding context offset.
-- Only returns the second indexes.
getDeBurjins' :: Data x => x -> Int -> Set Int
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
