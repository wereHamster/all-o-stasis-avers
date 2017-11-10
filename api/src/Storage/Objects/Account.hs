{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Account
    ( module Storage.Objects.Account.Types
    , accountObjectType
    , accountsView
    , createAdminAccount
    ) where

import Avers
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

adminAccount :: Account
adminAccount = Account "admin" Admin (Just "") (Just "")

createAdminAccount :: Avers ()
createAdminAccount = do
    accId <- Avers.createObject accountObjectType rootObjId adminAccount
    updateSecret (SecretId (unObjId accId)) "admin"
