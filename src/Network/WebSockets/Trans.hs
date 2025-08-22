{-# LANGUAGE
    FlexibleContexts
  #-}

-- |
-- Module      : Network.Wai.Trans
-- Copyright   : (c) 2015, 2016, 2017, 2018 Athan Clark
-- License     : BSD-style
-- Maintainer  : athan.clark@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Simple utilities for embedding a monad transformer stack in an 'Network.WebSockets.ClientApp'
-- or 'Network.WebSockets.ServerApp'.

module Network.WebSockets.Trans where

import Network.WebSockets (ConnectionOptions, ClientApp, ServerApp, Connection, PendingConnection)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Trans (MiddlewareT, runApplicationT, liftApplication)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO), askRunInIO, MonadIO (liftIO))


-- * Websockets

type ServerAppT m = PendingConnection -> m ()

liftServerApp :: MonadIO m
              => ServerApp -- ^ To lift
              -> ServerAppT m
liftServerApp s = liftIO . s

runServerAppT :: MonadUnliftIO m
              => ServerAppT m -- ^ To run
              -> m ServerApp
runServerAppT s = do
  toIO <- askRunInIO
  pure $ \pending -> toIO $ s pending

type ClientAppT m a = Connection -> m a

liftClientApp :: MonadIO m
              => ClientApp a -- ^ To lift
              -> ClientAppT m a
liftClientApp c = liftIO . c

runClientAppT :: MonadUnliftIO m
              => ClientAppT m a -- ^ To run
              -> m (ClientApp a)
runClientAppT c = do
  toIO <- askRunInIO
  pure $ \conn -> toIO $ c conn

-- * WAI Compatability

-- | Respond with the WebSocket server when applicable, as a middleware
websocketsOrT :: MonadUnliftIO m
              => ConnectionOptions
              -> ServerAppT m -- ^ Server
              -> MiddlewareT m
websocketsOrT cOpts server app req respond = do
  server' <- runServerAppT server
  app' <- runApplicationT app
  liftApplication (websocketsOr cOpts server' app') req respond
