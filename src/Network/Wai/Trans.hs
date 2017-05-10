{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , Rank2Types
  #-}

-- |
-- Module      : Network.Wai.Trans
-- Copyright   : (c) 2015 Athan Clark
-- License     : BSD-style
-- Maintainer  : athan.clark@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Simple utilities for embedding a monad transformer stack in an @Application@
-- or @Middleware@ - with @MiddlewareT@, your transformer stack is shared
-- across all attached middlewares until run. You can also lift existing @Middleware@
-- to @MiddlewareT@, given some extraction function.


module Network.Wai.Trans
  ( -- * WAI
    -- ** Types
    module Network.Wai
  , ApplicationT
  , MiddlewareT
  , -- ** Embedding
    liftApplication
  , liftMiddleware
  , runApplicationT
  , runMiddlewareT
  , hoistApplicationT
  , hoistMiddlewareT
  , inApplicationT
  , inMiddlewareT
  , -- ** Exception catching
    catchApplicationT
  , catchMiddlewareT
  , -- ** General Purpose
    readingRequest
  , -- * Websockets
    ServerAppT
  , liftServerApp
  , runServerAppT
  , ClientAppT
  , liftClientApp
  , runClientAppT
  , websocketsOrT
  ) where


import           Network.Wai
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets hiding (Request, Response)

import           Data.Singleton.Class (Extractable (..))
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad.Trans.Control.Aligned (MonadBaseControl (..))


-- * WAI

-- | Isomorphic to @Kleisli (ContT ResponseReceived m) Request Response@
type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived
type MiddlewareT m = ApplicationT m -> ApplicationT m

-- ** Generalization

liftApplication :: ( MonadBaseControl IO m stM
                   , Extractable stM
                   ) => Application -> ApplicationT m
liftApplication app req resp = liftBaseWith $ \runInBase -> app req (\r -> runSingleton <$> runInBase (resp r))

liftMiddleware :: ( MonadBaseControl IO m stM
                  , Extractable stM
                  , MonadIO m
                  ) => (forall a. m a -> IO a) -> Middleware -> MiddlewareT m
liftMiddleware run mid app = liftApplication (mid (runApplicationT run app))

runApplicationT :: MonadIO m => (forall a. m a -> IO a) -> ApplicationT m -> Application
runApplicationT run app req respond = run (app req (liftIO . respond))

runMiddlewareT :: ( MonadBaseControl IO m stM
                  , Extractable stM
                  , MonadIO m
                  ) => (forall a. m a -> IO a) -> MiddlewareT m -> Middleware
runMiddlewareT run mid app = runApplicationT run (mid (liftApplication app))

inApplicationT :: Monad m => m a -> ApplicationT m -> ApplicationT m
inApplicationT x app req resp = x >> app req resp

inMiddlewareT :: Monad m => m a -> MiddlewareT m -> MiddlewareT m
inMiddlewareT x mid = mid . inApplicationT x

-- ** Monad Morphisms

hoistApplicationT :: ( Monad m
                     , Monad n
                     ) => (forall a. m a -> n a)
                       -> (forall a. n a -> m a)
                       -> ApplicationT m
                       -> ApplicationT n
hoistApplicationT to from app req resp =
  to $ app req (from . resp)

hoistMiddlewareT :: ( Monad m
                    , Monad n
                    ) => (forall a. m a -> n a)
                      -> (forall a. n a -> m a)
                      -> MiddlewareT m
                      -> MiddlewareT n
hoistMiddlewareT to from mid =
  hoistApplicationT to from . mid . hoistApplicationT from to

-- ** Exception Catching

catchApplicationT :: ( MonadCatch m
                     , Exception e
                     ) => ApplicationT m -> (e -> ApplicationT m) -> ApplicationT m
catchApplicationT x f req respond =
  (x req respond) `catch` (\e -> f e req respond)

catchMiddlewareT :: ( MonadCatch m
                    , Exception e
                    ) => MiddlewareT m -> (e -> MiddlewareT m) -> MiddlewareT m
catchMiddlewareT x f app =
  (x app) `catchApplicationT` (\e -> f e app)

-- ** Utils

readingRequest :: Monad m => (Request -> m ()) -> MiddlewareT m
readingRequest f app req resp = do
  f req
  app req resp


-- * Websockets

type ServerAppT m = PendingConnection -> m ()

liftServerApp :: (MonadIO m) => ServerApp -> ServerAppT m
liftServerApp s = liftIO . s

runServerAppT :: (forall a. m a -> IO a) -> ServerAppT m -> ServerApp
runServerAppT run s = run . s

type ClientAppT m a = Connection -> m a

liftClientApp :: (MonadIO m) => ClientApp a -> ClientAppT m a
liftClientApp c = liftIO . c

runClientAppT :: (forall a. m a -> IO a) -> ClientAppT m a -> ClientApp a
runClientAppT run c = run . c


-- | Respond with the WebSocket server when applicable, as a middleware
websocketsOrT :: (MonadIO m) => (forall a. m a -> IO a) -> ConnectionOptions -> ServerAppT m -> MiddlewareT m
websocketsOrT run cOpts server app req respond =
  let server' pend = run $ server pend
      app' = liftApplication run . websocketsOr cOpts server' $ runApplicationT run app
  in  app' req respond
