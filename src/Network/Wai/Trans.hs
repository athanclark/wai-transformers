{-# LANGUAGE
    FlexibleContexts
  , Rank2Types
  #-}

-- |
-- Module      : Network.Wai.Trans
-- Copyright   : (c) 2015, 2016, 2017, 2018 Athan Clark
-- License     : BSD-style
-- Maintainer  : athan.clark@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Simple utilities for embedding a monad transformer stack in an 'Network.Wai.Application'
-- or 'Network.Wai.Middleware' - with 'MiddlewareT', your transformer stack is shared
-- across all attached middlewares until run. You can also lift existing 'Network.Wai.Middleware'
-- to 'MiddlewareT', given some extraction function.


module Network.Wai.Trans where

import Network.Wai (Application, Middleware, Request, Response, ResponseReceived)

import Control.Monad.Catch (Exception, MonadCatch (catch))
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO), askRunInIO, liftIO)


-- | Isomorphic to @Kleisli (ContT ResponseReceived m) Request Response@
type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived
type MiddlewareT m = ApplicationT m -> ApplicationT m

-- * Lift and Run

liftApplication :: MonadUnliftIO m
                => Application -- ^ To lift
                -> ApplicationT m
liftApplication app req respond = withRunInIO (\toIO -> app req (toIO . respond))

liftMiddleware :: MonadUnliftIO m
               => Middleware -- ^ To lift
               -> MiddlewareT m
liftMiddleware mid app req respond = do
  app' <- runApplicationT app
  withRunInIO (\toIO -> mid app' req (toIO . respond))

runApplicationT :: MonadUnliftIO m
                => ApplicationT m -- ^ To run
                -> m Application
runApplicationT app = do 
  toIO <- askRunInIO
  pure $ \req respond -> toIO . app req $ liftIO . respond

runMiddlewareT :: MonadUnliftIO m
               => MiddlewareT m -- ^ To run
               -> m Middleware
runMiddlewareT mid = do
  toIO <- askRunInIO
  pure $ \app req respond -> do
    app' <- toIO $ runApplicationT (mid (liftApplication app))
    app' req respond

-- * Monad Morphisms

hoistApplicationT :: Monad m
                  => Monad n
                  => (forall a. m a -> n a) -- ^ To
                  -> (forall a. n a -> m a) -- ^ From
                  -> ApplicationT m
                  -> ApplicationT n
hoistApplicationT to from app req resp =
  to $ app req (from . resp)

hoistMiddlewareT :: Monad m
                 => Monad n
                 => (forall a. m a -> n a) -- ^ To
                 -> (forall a. n a -> m a) -- ^ From
                 -> MiddlewareT m
                 -> MiddlewareT n
hoistMiddlewareT to from mid =
  hoistApplicationT to from . mid . hoistApplicationT from to

-- * Exception Catching

catchApplicationT :: MonadCatch m
                  => Exception e
                  => ApplicationT m
                  -> (e -> ApplicationT m) -- ^ Handler
                  -> ApplicationT m
catchApplicationT x f req respond =
  x req respond `catch` (\e -> f e req respond)

catchMiddlewareT :: MonadCatch m
                 => Exception e
                 => MiddlewareT m
                 -> (e -> MiddlewareT m) -- ^ Handler
                 -> MiddlewareT m
catchMiddlewareT x f app =
  x app `catchApplicationT` (`f` app)
