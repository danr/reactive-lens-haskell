{-# language RecordWildCards #-}
{-# language Rank2Types #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
module ReactiveLens where

import Control.Applicative
import Lens.Micro as L
import Lens.Micro.Extras
import Control.Monad.State as S

type SimpleLens a b = Lens a a b b

data Store m a = forall s . Store {
  lens :: SimpleLens s a,
  get :: m a,
  set :: a -> m (),
  transaction :: forall t . m t -> m t,
  listen :: m () -> m ()
}

via :: Monad m => SimpleLens a b -> Store m a -> Store m b
via ab (Store sa get set transaction listen) =
  Store
    (sa . ab)
    (view ab <$> get)
    (\ b -> set =<< L.set ab b <$> get)
    transaction
    listen

data ReactiveState m a = R {
  s :: a,
  pending :: Bool,
  depth :: Int,
  listeners :: [m ()]
}

initState :: forall a m . (MonadState (ReactiveState m a) m) => a -> Store m a
initState s0 = Store {
    lens = id,
    get = S.gets s,
    set = \ a -> modify (\ r -> r {s = a, pending = True}),
    transaction = transaction,
    listen = \ m -> modify (\ r@R{..} -> r {listeners = m:listeners})
  }
  where
  try_notify = do
    R{..} <- S.get
    when (depth == 0 && pending) $ do
      modify (\ r@R{..} -> r {pending = False})
      transaction $ sequence_ listeners

  transaction :: forall t . m t -> m t
  transaction m = do
    modify (\ r@R{..} -> r {depth = depth + 1})
    t <- m
    modify (\ r@R{..} -> r {depth = depth - 1})
    try_notify
    return t
