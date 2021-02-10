module Control.Monad.Freer.Fresh
  ( Fresh (..),
    runFresh,
    registerTypeVariables,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Range
import Tix.Types

data Fresh x where
  Fresh :: Fresh TypeVariable
  -- | Inclusive
  MinFreshFromNow :: Fresh TypeVariable

type Runner effect = forall effs a. Eff (effect ': effs) a -> Eff effs a

runFresh :: Runner Fresh
runFresh =
  evalState (TypeVariable 0)
    . reinterpret
      ( \case
          Fresh -> do
            (TypeVariable x) <- get
            put (TypeVariable $ x + 1)
            return (TypeVariable x)
          MinFreshFromNow -> get
      )

registerTypeVariables :: Member Fresh r => Eff r a -> Eff r (a, Range TypeVariable)
registerTypeVariables m = do
  l <- send MinFreshFromNow
  a <- m
  r <- send MinFreshFromNow
  return (a, Range l r)
