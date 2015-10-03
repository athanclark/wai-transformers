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
  ( module X
  , ApplicationT
  , MiddlewareT
  , liftApplication
  , liftMiddleware
  , runApplicationT
  , runMiddlewareT
  ) where


import           Network.Wai as X

import           Data.Function.Syntax
import           Control.Monad.IO.Class


type ApplicationT m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
type MiddlewareT m = ApplicationT m -> ApplicationT m


liftApplication :: MonadIO m => Application -> ApplicationT m
liftApplication app = liftIO .* app

liftMiddleware :: MonadIO m => (ApplicationT m -> Application) -> Middleware -> MiddlewareT m
liftMiddleware runAppT mid app = liftApplication $ mid (runAppT app)

runApplicationT :: (forall a. m a -> IO a) -> ApplicationT m -> Application
runApplicationT run app req respond = run (app req respond)

runMiddlewareT :: MonadIO m => (forall a. m a -> IO a) -> MiddlewareT m -> Middleware
runMiddlewareT run mid app req respond = run (mid (liftApplication app) req respond)

