{-# LANGUAGE UndecidableInstances #-}

module DSpies.Error.Combine
  ( CombinableErrors(..)
  , CombineT(..)
  , sequenceCombine
  , traverseCombine
  )
where

import           Prelude

import           Control.Monad.Except

class Applicative m => CombinableErrors m where
  (<*>+) :: m (x -> y) -> m x -> m y
  (*>+) :: m x -> m y -> m y
  (<*+) :: m x -> m y -> m x
infixl 4 <*>+
infixl 4 *>+
infixl 4 <*+

instance (Applicative m, MonadError err m, Semigroup err)
    => CombinableErrors m where
  (<*>+) = errOp (<*>)
  (*>+)  = errOp (*>)
  (<*+)  = errOp (<*)

errOp
  :: (MonadError err m, Semigroup err)
  => (m x -> m y -> m z)
  -> m x
  -> m y
  -> m z
errOp op x y =
  (x `catchError` \xerr ->
      (y `catchError` \yerr -> throwError $ xerr <> yerr) *> throwError xerr
    )
    `op` y

newtype CombineT m a = CombineT {unCombineT :: m a}
  deriving stock Functor

instance CombinableErrors m => Applicative (CombineT m) where
  pure = CombineT . pure
  (<*>) (CombineT x) (CombineT y) = CombineT $ x <*>+ y
  (*>) (CombineT x) (CombineT y) = CombineT $ x *>+ y
  (<*) (CombineT x) (CombineT y) = CombineT $ x <*+ y

traverseCombine
  :: (CombinableErrors m, Traversable t) => (x -> m y) -> t x -> m (t y)
traverseCombine fn = unCombineT . traverse (CombineT . fn)

sequenceCombine :: (CombinableErrors m, Traversable t) => t (m x) -> m (t x)
sequenceCombine = traverseCombine id
