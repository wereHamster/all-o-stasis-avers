{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Account
    ( module Storage.Objects.Account.Types
    , accountObjectType
    , accountsView
    , createAdminAccount
    ) where

import Avers hiding (Config)
import Config

import Control.Monad.State
import Storage.Objects.Account.Types

mkObjId :: Int -> Avers ObjId
mkObjId len = ObjId <$> liftIO (newId len)

accountViews :: [SomeView Account]
accountViews =
    [ SomeView accountsView
    ]

accountObjectType :: ObjectType Account
accountObjectType = ObjectType
    { otType   = "account"
    , otId     = mkObjId 42
    , otViews  = accountViews
    }

accountsView :: View Account Account
accountsView = View
    { viewName              = "accounts"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }

adminAccount :: Config -> Account
adminAccount config = Account "admin" Admin (Just (_cAdminAccountEmail config)) (Just "")

createAdminAccount :: Config -> Avers ()
createAdminAccount config = do
    accId <- Avers.createObject accountObjectType rootObjId (adminAccount config)
    updateSecret (SecretId (unObjId accId)) "admin"
