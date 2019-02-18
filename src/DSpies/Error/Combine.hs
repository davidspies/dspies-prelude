{-# LANGUAGE UndecidableInstances #-}

module DSpies.Error.Combine
  ( CombineT(..)
  , (<*>+)
  , (<*+)
  , (*>+)
  , sequenceCombine
  , traverseCombine
  )
where

import           Prelude

import           Control.Monad.Except

newtype CombineT m a = CombineT {unCombineT :: m a}
  deriving (Functor)

instance (Monad m, MonadError err m, Semigroup err)
    => Applicative (CombineT m) where
  pure = CombineT . pure
  (<*>) (CombineT x) (CombineT y) =
    CombineT
      $   (x `catchError` \xerr -> do
            _ <- y `catchError` \yerr -> throwError $ xerr <> yerr
            throwError xerr
          )
      <*> y

traverseCombine
  :: (MonadError err m, Monoid err, Traversable t)
  => (x -> m y)
  -> t x
  -> m (t y)
traverseCombine fn = unCombineT . traverse (CombineT . fn)

sequenceCombine
  :: (MonadError err m, Monoid err, Traversable t) => t (m x) -> m (t x)
sequenceCombine = traverseCombine id

opCombine
  :: (MonadError err m, Monoid err)
  => (CombineT m a -> CombineT m b -> CombineT m c)
  -> m a
  -> m b
  -> m c
opCombine op a b = unCombineT $ CombineT a `op` CombineT b

(<*>+) :: (MonadError err m, Monoid err) => m (x -> y) -> m x -> m y
(<*>+) = opCombine (<*>)
(*>+) :: (MonadError err m, Monoid err) => m x -> m y -> m y
(*>+) = opCombine (*>)
(<*+) :: (MonadError err m, Monoid err) => m x -> m y -> m x
(<*+) = opCombine (<*)
