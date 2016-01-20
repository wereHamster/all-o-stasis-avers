{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Types where


import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Servant.Server

import Avers

import Prelude


data AppState = AppState
    { _aversState :: Avers.Handle
    }

reqAvers2 :: Avers.Handle -> Avers a -> EitherT ServantErr IO a
reqAvers2 aversH m = do
    res <- liftIO $ evalAvers aversH m
    case res of
        Left e -> case e of
            DatabaseError _detail                  -> left $ err500 -- $ InternalServerError $ "database " <> detail
            NotAuthorized                          -> left $ err500 -- $ Unauthorized
            DocumentNotFound _                     -> left $ err500 -- $ NotFound
            UnknownObjectType _detail              -> left $ err500 -- $ InternalServerError $ "unknown object " <> detail
            ObjectNotFound _                       -> left $ err500 -- $ NotFound
            ParseError _ _detail                   -> left $ err500 -- $ InternalServerError $ "parse " <> detail
            PatchError (UnknownPatchError _detail) -> left $ err500 -- $ InternalServerError $ "patch " <> detail
            AversError _detail                     -> left $ err500 -- $ InternalServerError $ "avers " <> detail
            InternalError _ie                      -> left err500 -- (Left ie)
        Right r -> pure r

