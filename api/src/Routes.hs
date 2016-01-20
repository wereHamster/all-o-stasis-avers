{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}

module Routes (LocalAPI, serveLocalAPI) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Vector as V

import qualified Database.RethinkDB as R

import Avers as Avers
import Avers.TH
import Avers.API
import Avers.Server

import Servant.API hiding (Patch)
import Servant.Server

import Queries
import Revision
import Types

import Storage.ObjectTypes
import Storage.Objects.Account
import Storage.Objects.Activity
import Storage.Objects.Boulder

import Prelude


data SignupRequest2 = SignupRequest2
    { reqLogin :: Text
    }

data SignupResponse2 = SignupResponse2
    { _resObjId :: ObjId
    }


type LocalAPI
    = "revision"
      :> Get '[PlainText] Text

    :<|> "collection/ownedBoulders" :> "ownedBoulders"
      :> Credentials
      :> Get '[JSON] [ObjId]

    :<|> "collection/activeBoulders" :> "activeBoulders"
      :> Get '[JSON] [ObjId]

    :<|> "collection/adminAccounts" :> "adminAccounts"
      :> Credentials
      :> Get '[JSON] [ObjId]

    :<|> "signup"
      :> ReqBody '[JSON] SignupRequest2
      :> Post '[JSON] SignupResponse2


serveLocalAPI :: Avers.Handle -> Server LocalAPI
serveLocalAPI aversH =
         serveRevision
    :<|> serveOwnedBouldersCollection
    :<|> serveActiveBouldersCollection
    :<|> serveAdminAccounts
    :<|> serveSignup
  where
    serveRevision =
        pure $ T.pack $ fromMaybe "HEAD" $(revision)

    serveOwnedBouldersCollection cred = do
        ownerId <- credentialsObjId aversH cred
        objIds <- reqAvers2 aversH $ do
            -- FIXME: we should check if the setter is in the list of setters
            let isOwnBoulder :: R.Exp R.Object -> R.Exp Bool
                isOwnBoulder = \x -> R.Eq
                    (R.GetField "setter" x :: R.Exp Text)
                    (R.lift $ unObjId ownerId)

            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "timestamp"] $
                R.Filter isOwnBoulder $
                viewTable bouldersView

        pure $ map ObjId $ V.toList objIds

    serveActiveBouldersCollection = do
        boulders <- reqAvers2 aversH $ do
            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "timestamp"] $
                R.Filter isNotRemoved $
                viewTable bouldersView

        pure $ map ObjId $ V.toList boulders

    serveAdminAccounts cred = do
        ownerId <- credentialsObjId aversH cred
        objIds <- reqAvers2 aversH $ do
            let isSetter :: R.Exp R.Object -> R.Exp Bool
                isSetter = \x -> R.Ne
                    (R.GetField "role" x :: R.Exp Text)
                    ("user" :: R.Exp Text)

            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "name"] $
                R.Filter isSetter $
                viewTable accountsView

        pure $ map ObjId $ V.toList objIds

    serveSignup body = do
        let content = Account (reqLogin body) User (Just "") (Just "")

        accId <- reqAvers2 aversH $ do
            accId <- Avers.createObject accountObjectType rootObjId content
            updateSecret (SecretId (unObjId accId)) ""
            pure accId

        pure $ SignupResponse2 accId


$(deriveJSON (deriveJSONOptions "req")  ''SignupRequest2)
$(deriveJSON (deriveJSONOptions "_res") ''SignupResponse2)
