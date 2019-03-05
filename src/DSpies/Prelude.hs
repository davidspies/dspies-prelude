module DSpies.Prelude
  ( (.:)
  , (<&>)
  , eitherErrorShowId
  , firstF
  , fromJust
  , mapErrors
  , secondF
  , zipExact
  , zipExactWith
  , module X
  )
where

import           Prelude                       as X

import           Control.Applicative           as X
import           Control.Arrow                 as X
                                                ( (***)
                                                , (&&&)
                                                , (>>>)
                                                )
import           Control.Monad                 as X
import           Control.Monad.IO.Class        as X
import           Control.Monad.Except          as X
                                                ( MonadError
                                                , ExceptT
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.Extra           as X
import           Control.Monad.Reader          as X
                                                ( MonadReader
                                                , Reader
                                                , ReaderT
                                                , runReader
                                                , runReaderT
                                                )
import           Control.Monad.State           as X
                                                ( MonadState
                                                , State
                                                , StateT
                                                , evalState
                                                , evalStateT
                                                , execState
                                                , execStateT
                                                , runState
                                                , runStateT
                                                )
import           Control.Monad.Trans           as X
                                                ( MonadTrans
                                                , lift
                                                )
import           Control.Monad.Writer          as X
                                                ( MonadWriter
                                                , Writer
                                                , WriterT
                                                , execWriter
                                                , execWriterT
                                                , runWriter
                                                , runWriterT
                                                )
import           Data.Bifunctor                as X
import           Data.Constraint               as X
                                                ( Constraint
                                                , Dict(..)
                                                )
import           Data.Foldable                 as X
import           Data.Function                 as X
import           Data.Functor.Compose          as X
import           Data.Functor.Identity         as X
import           Data.Kind                     as X
                                                ( Type )
import           Data.DList                    as X
                                                ( DList )
import           Data.List                     as X
import           Data.List.NonEmpty            as X
                                                ( NonEmpty(..)
                                                , nonEmpty
                                                )
import           Data.Map                      as X
                                                ( Map )
import           Data.Maybe                    as X
                                         hiding ( fromJust )
import           Data.Proxy                    as X
import           Data.Set                      as X
                                                ( Set )
import           Data.String                   as X
                                                ( IsString )
import           Data.Text                     as X
                                                ( Text )
import           Data.Void                     as X
import           GHC.Generics                  as X
                                                ( Generic )
import           GHC.Stack                     as X
                                                ( HasCallStack )

import           DSpies.Error.Combine          as X
import           DSpies.Error.DList            as X
import           DSpies.FWrapped               as X
import           DSpies.Monad.Transformed      as X

firstF :: Functor f => (a -> f c) -> (a, b) -> f (c, b)
firstF fn (x, y) = (, y) <$> fn x

secondF :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
secondF fn (x, y) = (x, ) <$> fn y

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixr 4 <&>

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:

fromJust :: HasCallStack => Maybe a -> a
fromJust = fromMaybe (error "Maybe.fromJust: Nothing")

eitherErrorShowId :: (HasCallStack, Show err) => Either err a -> a
eitherErrorShowId = either (error . show) id

zipExactWith :: HasCallStack => (a -> b -> c) -> [a] -> [b] -> [c]
zipExactWith fn = go
 where
  go []       []       = []
  go (_ : _)  []       = error "Mismatched zip lengths"
  go []       (_ : _ ) = error "Mismatched zip lengths"
  go (x : xs) (y : ys) = fn x y : go xs ys

zipExact :: HasCallStack => [a] -> [b] -> [(a, b)]
zipExact = zipExactWith (,)

mapErrors :: (MonadError err m) => (errsub -> err) -> ExceptT errsub m a -> m a
mapErrors errfn act = runExceptT act >>= \case
  Left  err -> throwError $ errfn err
  Right val -> return val
