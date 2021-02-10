module Control.Monad.Freer.TreeTracer
  ( TreeTracer (..),
    traceValue,
    traceSubtree,
    runTreeTracer,
    runNoTreeTracer,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.Internal (handleRelayS)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Vector as V

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
