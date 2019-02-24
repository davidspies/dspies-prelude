module DSpies.FWrapped where

import           Control.Applicative            ( liftA2 )
import           Prelude

-- | For use with DerivingVia
newtype FWrapped f a = FWrapped (f a)
  deriving (Functor, Applicative)

instance (Applicative f, Num a) => Num (FWrapped f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  negate      = fmap negate
  (-)         = liftA2 (-)
  fromInteger = pure . fromInteger

instance (Applicative f, Fractional a) => Fractional (FWrapped f a) where
  fromRational = pure . fromRational
  recip        = fmap recip
  (/)          = liftA2 (/)

instance (Applicative f, Floating a) => Floating (FWrapped f a) where
  pi      = pure pi
  exp     = fmap exp
  log     = fmap log
  sqrt    = fmap sqrt
  (**)    = liftA2 (**)
  logBase = liftA2 logBase
  sin     = fmap sin
  cos     = fmap cos
  tan     = fmap tan
  asin    = fmap asin
  acos    = fmap acos
  atan    = fmap atan
  sinh    = fmap sinh
  cosh    = fmap cosh
  tanh    = fmap tanh
  asinh   = fmap asinh
  acosh   = fmap acosh
  atanh   = fmap atanh

instance (Applicative f, Bounded a) => Bounded (FWrapped f a) where
  minBound = pure minBound
  maxBound = pure maxBound

instance (Applicative f, Semigroup a) => Semigroup (FWrapped f a) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (FWrapped f a) where
  mempty  = pure mempty
  mappend = liftA2 mappend
  mconcat = fmap mconcat . sequenceA
