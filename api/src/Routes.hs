{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}

module Routes (LocalAPI, serveLocalAPI) where

import Control.Monad.Except

import Data.Maybe
import Data.Text (Text)
import Data.Aeson (Value)
import qualified Data.Text as T

import qualified Data.Vector as V

import qualified Database.RethinkDB as R

import Avers as Avers
import Avers.TH
import Avers.API
import Avers.Server

import Servant.API hiding (Patch)
import Servant.Server

import Authorization
import Queries
import Revision
import Types

import Storage.ObjectTypes
import Storage.Objects.Account
import Storage.Objects.Boulder

import Prelude


data SignupRequest = SignupRequest
    { reqLogin :: Text
    }

data SignupResponse = SignupResponse
    { _resObjId :: ObjId
    }

data ChangeSecretRequest = ChangeSecretRequest
    { reqId     :: ObjId
    , reqSecret :: Text
    }


type LocalAPI
    -- server the git revsion sha
    = "revision"
      :> Get '[PlainText] Text

    -- serve a list of all active bouldersIds in the gym
    :<|> "collection" :> "activeBoulders"
      :> Get '[JSON] [ObjId]

    -- serve a list of boulderIds that are owned/authored by the user
    :<|> "collection" :> "ownBoulders"
      :> Credentials
      :> Get '[JSON] [ObjId]

    -- serve a list of all accountIds
    :<|> "collection" :> "accounts"
      :> Get '[JSON] [ObjId]

    -- serve a list of all non-user accountIds
    :<|> "collection" :> "adminAccounts"
      :> Credentials
      :> Get '[JSON] [ObjId]

    :<|> "signup"
      :> ReqBody '[JSON] SignupRequest
      :> Post '[JSON] SignupResponse

    :<|> "updateSecret"
      :> Credentials
      :> ReqBody '[JSON] ChangeSecretRequest
      :> Post '[JSON] SignupResponse

    :<|> "objects"
      :> Credentials
      :> ReqBody '[JSON] Avers.API.CreateObjectBody
      :> Post '[JSON] Avers.API.CreateObjectResponse

serveLocalAPI :: Avers.Handle -> Server LocalAPI
serveLocalAPI aversH =
         serveRevision
    :<|> serveActiveBouldersCollection
    :<|> serveOwnBouldersCollection
    :<|> serveAccounts
    :<|> serveAdminAccounts
    :<|> serveSignup
    :<|> serveUpdateSecret
    :<|> objects
  where
    serveRevision =
        pure $ T.pack $ fromMaybe "HEAD" $(revision)

    serveActiveBouldersCollection = do
        boulders <- reqAvers2 aversH $ do
            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "setDate"] $
                R.Filter isNotRemoved $
                viewTable bouldersView

        pure $ map ObjId $ V.toList boulders

    serveOwnBouldersCollection cred = do
        ownerId <- credentialsObjId aversH cred
        objIds <- reqAvers2 aversH $ do
            -- FIXME: we should check if the setter is in the list of setters
            let isOwnBoulder :: R.Exp R.Object -> R.Exp Bool
                isOwnBoulder = \x -> R.Eq
                    (R.GetField "setter" x :: R.Exp Text)
                    (R.lift $ unObjId ownerId)

            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "setDate"] $
                R.Filter isOwnBoulder $
                viewTable bouldersView

        pure $ map ObjId $ V.toList objIds

    serveAccounts = do
        objIds <- reqAvers2 aversH $ do
            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "name"] $
                viewTable accountsView

        pure $ map ObjId $ V.toList objIds

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

        pure $ SignupResponse accId

    serveUpdateSecret cred body = do
        ownerId <- credentialsObjId aversH cred
        accId <- reqAvers2 aversH $ do
            updateSecret (SecretId (unObjId ownerId)) (reqSecret body)
            pure ownerId

        pure $ SignupResponse accId

    objects cred body = do
        sessionId <- credentialsObjId aversH cred
        objId <- reqAvers2 aversH $ do
            authorizeObjectCreate sessionId (cobType body)

            (SomeObjectType objType) <- Avers.lookupObjectType (cobType body)
            content <- case parseValueAs objType (cobContent body) of
                Left e -> throwError e
                Right x -> pure x

            objId <- Avers.createObject objType sessionId content
            pure objId

        pure $ Avers.API.CreateObjectResponse objId (cobType body) (cobContent body)



$(deriveJSON (deriveJSONOptions "req")  ''SignupRequest)
$(deriveJSON (deriveJSONOptions "_res") ''SignupResponse)
$(deriveJSON (deriveJSONOptions "req")  ''ChangeSecretRequest)

