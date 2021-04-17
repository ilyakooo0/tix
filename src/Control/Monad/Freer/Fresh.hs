module Control.Monad.Freer.Fresh
  ( Fresh (..),
    fresh,
    freshSub,
    runFresh,
    registerTypeVariables,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.State
import qualified Data.RangeSet.Map as RS
import Tix.Types

data Fresh x where
  Fresh :: Fresh TypeVariable
  FreshSub :: TypeVariable -> Fresh TypeVariable
  -- | Inclusive
  MinFreshFromNow :: Fresh RangeTypeVariable

fresh :: Member Fresh eff => Eff eff TypeVariable
fresh = send Fresh
{-# INLINE fresh #-}

freshSub :: Member Fresh eff => TypeVariable -> Eff eff TypeVariable
freshSub = send . FreshSub
{-# INLINE freshSub #-}

type Runner effect = forall effs a. Eff (effect ': effs) a -> Eff effs a

runFresh :: Runner Fresh
runFresh =
  evalState (RangeTypeVariable 0)
    . reinterpret
      ( \case
          Fresh -> do
            (RangeTypeVariable x) <- get
            put (RangeTypeVariable $ x + 1)
            return (TypeVariable x)
          FreshSub tv -> do
            (RangeTypeVariable x) <- get
            put (RangeTypeVariable $ x + 1)
            return (SubTypeVariable (unRangeTypeVariable $ rangeTypeVariable tv) x)
          MinFreshFromNow -> get
      )
{-# INLINE runFresh #-}

registerTypeVariables :: Member Fresh r => Eff r a -> Eff r (a, RS.RSet RangeTypeVariable)
registerTypeVariables m = do
  l <- send MinFreshFromNow
  a <- m
  r <- send MinFreshFromNow
  return (a, RS.singletonRange (l, pred r))
