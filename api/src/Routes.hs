{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes (routes) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Data.ByteString.Lazy   as BL

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Vector as V

import Data.Aeson (Value, (.=), object)

import Data.IORef
import Data.Time

import qualified Database.RethinkDB as R

import Avers as Avers
import Avers.TH

import Snap

import Types
import Session

import Storage.ObjectTypes
import Storage.Objects.Account
import Storage.Objects.Boulder


mapId:: R.Exp R.Object -> R.Exp Text
mapId = R.GetField "id"

isNotDeleted :: R.Exp R.Object -> R.Exp Bool
isNotDeleted x = R.Any
    [ R.Not $ R.HasFields ["deleted"] x
    , R.Eq
        (R.GetField "deleted" x :: R.Exp Bool)
        (R.lift False)
    ]


createSessionHandler :: RequestHandler ()
createSessionHandler = do
    csr <- parseRequestBody
    createLocalSession csr


lookupSessionHandler :: RequestHandler ()
lookupSessionHandler = do
    cookie <- getSessionCookie
    case cookie of
        Nothing -> modifyResponse $ setResponseCode 404
        Just Cookie{..} -> do
            sessionDoc <- reqAvers $ lookupSession (SessionId $ T.decodeUtf8 cookieValue)
            case sessionDoc of
                Left _ -> modifyResponse $ setResponseCode 404
                Right session -> do
                    touchSessionCookie
                    sendResponse $ ClientSession (unSessionId $ sessionId session) (unObjId $ sessionObjId session)


deleteSessionHandler :: RequestHandler ()
deleteSessionHandler = do
    cookie <- getSessionCookie
    expireSessionCookie

    case cookie of
        Nothing -> return ()
        Just Cookie{..} -> void $ reqAvers $ dropSession (SessionId $ T.decodeUtf8 cookieValue)



objAndRevIds :: RequestHandler (ObjId, RevId)
objAndRevIds = (,) <$> (textParam "objId" >>= asObjId) <*> (textParam "revId" >>= asRevId)


-----------------------------------------------------------------------------
-- Collection: :objectType

objectsOfTypeCollectionRoute :: Text -> ObjectType a -> Route
objectsOfTypeCollectionRoute name objType =
    ( GET
    , "collection/" <> name
    , objectsOfTypeCollectionHandler objType
    )

objectsOfTypeCollectionHandler :: ObjectType a -> RequestHandler ()
objectsOfTypeCollectionHandler objType = do
    res <- reqAvers $ Avers.objectsOfType objType
    replyWith res (sendResponse . V.toList)



-----------------------------------------------------------------------------
-- Collection: own Boulders

ownedBouldersCollectionRoute :: Route
ownedBouldersCollectionRoute =
    ( GET
    , "collection/ownedBoulders"
    , ownBoulderCollectionHandler
    )

ownBoulderCollectionHandler :: RequestHandler ()
ownBoulderCollectionHandler = do
    session <- requireSession

    objIds <- reqAvers $ do
        {-let predicate :: R.Exp R.Object -> R.Exp Bool-}
            {-predicate = \x -> R.Eq-}
                {-(R.GetField "setter" x :: R.Exp Text)-}
                {-(R.lift $ unObjId $ sessionObjId session)-}

        runQueryCollect $
            R.Map mapId $
            R.OrderBy [R.Descending "id"] $
            viewTable bouldersView

    case objIds :: Either AversError (V.Vector Text) of
        Right x -> sendResponse $ map ObjId $ V.toList x
        Left e -> do
            liftIO $ print e
            failWith NotFound

-----------------------------------------------------------------------------
-- Collection: batteryEvents

{-
batteryEventsCollectionRoute :: Route
batteryEventsCollectionRoute =
    ( GET
    , "collection/batteryEvents/:batteryId"
    , batteryEventsCollectionHandler
    )

batteryEventsCollectionHandler :: RequestHandler ()
batteryEventsCollectionHandler = do
    session <- requireSession
    batteryId <- asObjId =<< textParam "batteryId"

    objIds <- reqAvers $ do
        let predicate :: R.Exp R.Object -> R.Exp Bool
            predicate = \x -> R.Eq
                (R.GetField "batteryId" x :: R.Exp Text)
                (R.lift $ unObjId $ batteryId)

        runQueryCollect $
            R.Map mapId $
            R.OrderBy [R.Ascending "timestamp"] $
            R.Filter isNotDeleted $
            R.Filter predicate $
            viewTable eventsView

    case objIds :: Either AversError (V.Vector Text) of
        Right x -> sendResponse $ map ObjId $ V.toList x
        Left e -> do
            liftIO $ print e
            failWith NotFound

-}


data CreateObjectRequest = CreateObjectRequest
  { reqType    :: Text
  , reqContent :: Value
  }

data CreateObjectResponse = CreateObjectResponse
  { _coresId      :: ObjId
  , _coresType    :: Text
  , _coresContent :: Value
  }


createObjectHandler :: RequestHandler ()
createObjectHandler = do
    session <- requireSession

    CreateObjectRequest{..} <- parseRequestBody
    objId <- reqAvers $ do
        -- authorizeObjectCreate session reqType

        (SomeObjectType ot) <- lookupObjectType reqType
        content <- case parseValueAs ot reqContent of
            Left e -> throwError e
            Right x -> return x

        Avers.createObject ot (sessionObjId session) content

    case objId of
        Left  e -> error (show e)
        Right x -> sendResponse $ CreateObjectResponse x reqType reqContent


createObjectHandler' :: RequestHandler ()
createObjectHandler' = do
    session <- requireSession

    CreateObjectRequest{..} <- parseRequestBody
    objId <- asObjId =<< textParam "objId"
    res <- reqAvers $ do
        -- authorizeObjectCreate session reqType

        (SomeObjectType ot) <- lookupObjectType reqType
        content <- case parseValueAs ot reqContent of
            Left e -> throwError e
            Right x -> return x

        now <- liftIO getCurrentTime
        Avers.createObject' objId now ot (sessionObjId session) content

    case res of
        Left  e  -> error (show e)
        Right () -> sendResponse $ CreateObjectResponse objId reqType reqContent





data LookupObjectResponse = LookupObjectResponse
  { _loresId         :: ObjId
  , _loresType       :: Text
  , _loresCreatedAt  :: UTCTime
  , _loresCreatedBy  :: ObjId
  , _loresRevisionId :: RevId
  , _loresContent    :: Value
  }



lookupObjectHandler :: RequestHandler ()
lookupObjectHandler = do
    objId <- asObjId =<< textParam "objId"

    res <- reqAvers $ do
        object   <- Avers.lookupObject objId
        snapshot <- Avers.lookupLatestSnapshot (BaseObjectId objId)

        return (object, snapshot)

    replyWith res $ \(Object{..}, Snapshot{..}) ->
        sendResponse $ LookupObjectResponse
            objId
            objectType
            objectCreatedAt
            objectCreatedBy
            snapshotRevisionId
            snapshotContent



data PatchObjectRequest = PatchObjectRequest
  { reqRevisionId :: RevId
    -- ^ The 'RevId' against which the client created the operations. This may
    -- be a bit behind if some other client submitted patches in parallel.

  , reqOperations :: [ Operation ]
    -- ^ The operations which the client wants to store in the database.
  }


data PatchObjectResponse = PatchObjectResponse
  { _poresPreviousPatches :: ![Patch]
    -- ^ Patches which were already in the database. The submitted ops were
    -- rebased on top of these.

  , _poresNumProcessedOperations :: !Int
    -- ^ The number of operations which were processed. This may be smaller
    -- than the number of submitted ops if the processing failed somewhere
    -- in the middle. The client can then decide what to do with those which
    -- were not accepted.

  , _poresResultingPatches :: ![Patch]
    -- ^ Out of the submitted operations, these are the patches which were
    -- actually applied and stored in the database. This list may be shorter
    -- if some operations were dropped (because redundant or conflicting).
  }



patchObjectHandler :: RequestHandler ()
patchObjectHandler = do
    session <- requireSession

    objId <- asObjId =<< textParam "objId"
    PatchObjectRequest{..} <- parseRequestBody

    res <- reqAvers $ do
        -- authorizePatch session objId

        applyObjectUpdates
            (BaseObjectId objId)
            reqRevisionId
            (sessionObjId session)
            reqOperations
            False

    replyWith res $ \(previousPatches, numProcessedOperations, resultingPatches) -> do
        sendResponse $ PatchObjectResponse
            previousPatches numProcessedOperations resultingPatches



deleteObjectHandler :: RequestHandler ()
deleteObjectHandler = do
    session <- requireSession
    objId   <- asObjId =<< textParam "objId"

    res <- reqAvers $ do
        --authorizeObjectDelete session objId
        Avers.deleteObject objId
    replyWith res sendResponse



data SignupRequest = SignupRequest
  { reqLogin :: Text
  }

data SignupResponse = SignupResponse
  { _resObjId :: ObjId
  }



signupHandler :: RequestHandler()
signupHandler = do
    SignupRequest{..} <- parseRequestBody
    let content = Account reqLogin "" "" User

    res <- reqAvers $ do
        accId <- Avers.createObject accountObjectType rootObjId content
        updateSecret (SecretId (unObjId accId)) ""
        return accId

    replyWith res (sendResponse . SignupResponse)



routes :: [Route]
routes =
    [ ( POST,   "objects",                         createObjectHandler)
    , ( POST,   "objects/:objId",                  createObjectHandler')
    , ( GET,    "objects/:objId",                  lookupObjectHandler)
    , ( PATCH,  "objects/:objId",                  patchObjectHandler)
    , ( DELETE, "objects/:objId",                  deleteObjectHandler)

    , ( POST,   "session",                         createSessionHandler)
    , ( GET,    "session",                         lookupSessionHandler)
    , ( DELETE, "session",                         deleteSessionHandler)

    , ( POST,   "signup",                          signupHandler)
    -- , ( POST,   "secret",                          changeSecretHandler)

    , ownedBouldersCollectionRoute

    {-, objectsOfTypeCollectionRoute "batteries" batteryObjectType-}
    ]



$(deriveJSON (deriveJSONOptions "req") ''CreateObjectRequest)
$(deriveJSON (deriveJSONOptions "_cores") ''CreateObjectResponse)
$(deriveJSON (deriveJSONOptions "_lores") ''LookupObjectResponse)
$(deriveJSON (deriveJSONOptions "req") ''PatchObjectRequest)
$(deriveJSON (deriveJSONOptions "_pores") ''PatchObjectResponse)
$(deriveJSON (deriveJSONOptions "req")  ''SignupRequest)
$(deriveJSON (deriveJSONOptions "_res") ''SignupResponse)
