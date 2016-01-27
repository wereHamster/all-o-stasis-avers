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
    -- server the git revsion sha
    = "revision"
      :> Get '[PlainText] Text

    -- serve a list of all activities
    :<|> "collection" :> "activites"
      :> Get '[JSON] [ObjId]

    -- serve a list of activityIds that involve the actor
    :<|> "collection" :> "actorActivities"
      :> Capture "objId" ObjId
      :> Get '[JSON] [ObjId]

    -- serve a list of activityIds that involve the object
    :<|> "collection" :> "objectActivities"
      :> Capture "objId" ObjId
      :> Get '[JSON] [ObjId]

    -- serve a list of all active bouldersIds in the gym
    :<|> "collection" :> "activeBoulders"
      :> Get '[JSON] [ObjId]

    -- serve a list of boulderIds that are owned/authored by the user
    :<|> "collection" :> "ownBoulders"
      :> Credentials
      :> Get '[JSON] [ObjId]

    -- serve a list of all non-user accountIds
    :<|> "collection" :> "adminAccounts"
      :> Credentials
      :> Get '[JSON] [ObjId]

    :<|> "signup"
      :> ReqBody '[JSON] SignupRequest2
      :> Post '[JSON] SignupResponse2


serveLocalAPI :: Avers.Handle -> Server LocalAPI
serveLocalAPI aversH =
         serveRevision
    :<|> serveActivitiesCollection
    :<|> serveActorActivitiesCollection
    :<|> serveObjectActivitiesCollection
    :<|> serveActiveBouldersCollection
    :<|> serveOwnBouldersCollection
    :<|> serveAdminAccounts
    :<|> serveSignup
  where
    serveRevision =
        pure $ T.pack $ fromMaybe "HEAD" $(revision)

    serveActivitiesCollection = do
        activities <- reqAvers2 aversH $ do
            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "timestamp"] $
                R.Filter isNotRemoved $
                viewTable activitiesView

        pure $ map ObjId $ V.toList activities

    serveActorActivitiesCollection actorId = do
        objIds <- reqAvers2 aversH $ do
            let isActor :: R.Exp R.Object -> R.Exp Bool
                isActor = \x -> R.Eq
                    (R.GetField "actor" x :: R.Exp Text)
                    (R.lift $ unObjId actorId)

            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "timestamp"] $
                R.Filter isActor $
                viewTable activitiesView

        pure $ map ObjId $ V.toList objIds

    serveObjectActivitiesCollection objectId = do
        objIds <- reqAvers2 aversH $ do
            let isObject :: R.Exp R.Object -> R.Exp Bool
                isObject = \x -> R.Eq
                    (R.GetField "object" x :: R.Exp Text)
                    (R.lift $ unObjId objectId)

            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "timestamp"] $
                R.Filter isObject $
                viewTable activitiesView

        pure $ map ObjId $ V.toList objIds

    serveActiveBouldersCollection = do
        boulders <- reqAvers2 aversH $ do
            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "timestamp"] $
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
                R.OrderBy [R.Descending "timestamp"] $
                R.Filter isOwnBoulder $
                viewTable bouldersView

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

        pure $ SignupResponse2 accId


$(deriveJSON (deriveJSONOptions "req")  ''SignupRequest2)
$(deriveJSON (deriveJSONOptions "_res") ''SignupResponse2)
