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
  ( -- * Types
    module Network.Wai
  , ApplicationT
  , MiddlewareT
  , liftApplication
  , liftMiddleware
  , runApplicationT
  , runMiddlewareT
  , -- * Exception catching
    catchApplicationT
  , catchMiddlewareT
  ) where


import           Network.Wai

import           Control.Monad.IO.Class
import           Control.Monad.Catch

-- | Isomorphic to @Kleisli (ContT ResponseReceived m) Request Response@
type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived
type MiddlewareT m = ApplicationT m -> ApplicationT m


liftApplication :: MonadIO m => (forall a. m a -> IO a) -> Application -> ApplicationT m
liftApplication run app req resp = liftIO (app req (run . resp))

liftMiddleware :: MonadIO m => (forall a. m a -> IO a) -> Middleware -> MiddlewareT m
liftMiddleware run mid app = liftApplication run (mid (runApplicationT run app))

runApplicationT :: MonadIO m => (forall a. m a -> IO a) -> ApplicationT m -> Application
runApplicationT run app req respond = run (app req (liftIO . respond))

runMiddlewareT :: MonadIO m => (forall a. m a -> IO a) -> MiddlewareT m -> Middleware
runMiddlewareT run mid app = runApplicationT run (mid (liftApplication run app))


catchApplicationT :: ( MonadCatch m
                     , Exception e
                     ) => ApplicationT m -> (e -> ApplicationT m) -> ApplicationT m
catchApplicationT x f req respond =
  (x req respond) `catch` (\e -> f e req respond)

catchMiddlewareT :: ( MonadIO m
                    , MonadCatch m
                    , Exception e
                    ) => MiddlewareT m -> (e -> MiddlewareT m) -> MiddlewareT m
catchMiddlewareT x f app =
  (x app) `catchApplicationT` (\e -> f e app)

