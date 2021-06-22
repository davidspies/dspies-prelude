{-# LANGUAGE UndecidableInstances #-}

module DSpies.Monad.Transformed
  ( Transformed (..),
    TransStateApply (..),
    TransStateFunctor (..),
    TransStatePass (..),
    liftWithCarry,
  )
where

import Control.Monad ((<=<))
import Control.Monad.Cont (MonadCont)
import qualified Control.Monad.Cont as Cont
import Control.Monad.Except
  ( ExceptT,
    MonadError (..),
  )
import Control.Monad.IO.Class
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    reader,
  )
import qualified Control.Monad.Reader as Reader
import Control.Monad.State
  ( MonadState,
    StateT,
  )
import qualified Control.Monad.State as State
import Control.Monad.Trans
  ( MonadTrans,
    lift,
  )
import Control.Monad.Trans.Control
import Control.Monad.Writer
  ( MonadWriter,
    WriterT,
    writer,
  )
import qualified Control.Monad.Writer as Writer
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Prelude

-- | For use with DerivingVia
newtype Transformed t (m :: Type -> Type) a = Transformed {unTransformed :: t m a}
  deriving (Functor, Applicative, Monad, MonadIO) via (t m)
  deriving
    ( MonadTrans,
      MonadTransControl,
      TransStateFunctor,
      TransStatePass,
      TransStateApply
    )
    via t

liftWithCarry ::
  (MonadTransControl t, Monad m, Monad (t m)) =>
  (Run t -> m (StT t b)) ->
  t m b
liftWithCarry act = liftWith act >>= restoreT . return

instance
  (MonadTransControl t, Monad (t m), MonadReader r m) =>
  MonadReader r (Transformed t m)
  where
  ask = lift Reader.ask
  local fn act = liftWithCarry (\run -> Reader.local fn (run act))
  reader = lift . reader

instance
  (MonadTransControl t, MonadError e m, Monad (t m)) =>
  MonadError e (Transformed t m)
  where
  throwError = lift . throwError
  catchError act onErr =
    liftWithCarry (\run -> run act `catchError` (run . onErr))

class TransStateFunctor t where
  fmapTransState :: Proxy t -> (a -> b) -> StT t a -> StT t b

class TransStatePass t where
  pullOutTransformation ::
    Monoid w => Proxy t -> Proxy a -> StT t (a, w -> w) -> (StT t a, w -> w)

class TransStateApply t where
  applyTransState :: Monad m => a -> t m (StT t a)

instance
  ( MonadTransControl t,
    MonadWriter w m,
    Monad (t m),
    TransStateFunctor t,
    TransStatePass t
  ) =>
  MonadWriter w (Transformed t m)
  where
  writer = lift . writer
  tell = lift . Writer.tell
  listen (act :: Transformed t m a) = liftWithCarry $ \run ->
    (\(x, y) -> fmapTransState @t @a Proxy (,y) x) <$> Writer.listen (run act)
  pass (act :: Transformed t m (a, w -> w)) = liftWithCarry $ \run ->
    Writer.pass (pullOutTransformation (Proxy @t) (Proxy @a) <$> run act)

instance
  (MonadTrans t, MonadState s m, Monad (t m)) =>
  MonadState s (Transformed t m)
  where
  get = lift State.get
  put = lift . State.put
  state = lift . State.state

instance
  (MonadTransControl t, MonadCont m, Monad (t m), TransStateApply t) =>
  MonadCont (Transformed t m)
  where
  callCC act = liftWithCarry $
    \run -> Cont.callCC (\c -> run (act (lift . c <=< applyTransState)))

instance TransStateFunctor (ReaderT r) where
  fmapTransState _ = id

instance TransStateFunctor (ExceptT e) where
  fmapTransState _ = fmap

instance TransStateFunctor (WriterT w) where
  fmapTransState _ = first

instance TransStateFunctor (StateT s) where
  fmapTransState _ = first

instance TransStatePass (ReaderT r) where
  pullOutTransformation _ _ = id

instance TransStatePass (ExceptT e) where
  pullOutTransformation _ _ = \case
    Left err -> (Left err, id)
    Right (v, op) -> (Right v, op)

instance TransStatePass (WriterT w) where
  pullOutTransformation _ _ ((x, op), w) = ((x, w), op)

instance TransStatePass (StateT s) where
  pullOutTransformation _ _ ((x, op), s) = ((x, s), op)

instance TransStateApply (ReaderT r) where
  applyTransState = return

instance TransStateApply (ExceptT e) where
  applyTransState = return . Right

instance Monoid w => TransStateApply (WriterT w) where
  applyTransState = return . (,mempty)

instance TransStateApply (StateT s) where
  applyTransState = State.gets . (,)
