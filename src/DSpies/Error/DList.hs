module DSpies.Error.DList
  ( ExceptTDL,
    runExceptTDL,
    throwErrorElem,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.DList (DList)
import qualified Data.DList as DList
import Prelude

type ExceptTDL err = ExceptT (DList err)

throwErrorElem :: MonadError (DList err) m => err -> m y
throwErrorElem = throwError . DList.singleton

runExceptTDL :: Functor m => ExceptTDL err m a -> m (Either [err] a)
runExceptTDL = fmap (first DList.toList) . runExceptT
