{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Authorization (aosAuthorization) where

import           Avers                  as Avers
import           Avers.API
import           Avers.Server

import qualified Data.Vector            as V
import qualified Database.RethinkDB     as R

import           Prelude

import           Queries
import           Storage.Objects.Account
import           Storage.Objects.Boulder


aosAuthorization :: Avers.Server.Authorizations
aosAuthorization = Avers.Server.Authorizations
    { createObjectAuthz = \cred objId ->
        [ sufficient $ return (objId == "account")
        , sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            isSet <- sessionIsSetter session
            isAdm <- sessionIsAdmin session
            return $ isSet || isAdm
        , pure RejectR
        ]
    , lookupObjectAuthz = \cred objId ->
        [ sufficient $ do
            objectIsBoulder objId
        , sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            hasCreated <- sessionCreatedObject session objId 
            isAdm <- sessionIsAdmin session
            return $ hasCreated || isAdm
        --, pure RejectR
        ]
    , patchObjectAuthz = \cred objId ops ->
        [ sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            isObj <- sessionIsObject session objId
            hasCreated <- sessionCreatedObject session objId 
            isAdm <- sessionIsAdmin session
            return $ isObj || hasCreated || isAdm
        --, sufficient $ do
        --    oper <- case ops of
        --        Set{..} -> opPath /= "role"         -- only admins can patch the 'role' field
        --        _       -> False
        --    return oper
        -- setters can patch objects they own or are setters
        ]
    , deleteObjectAuthz = \_ _ -> [pure RejectR]
    , uploadBlobAuthz = \_ _ -> [pure AllowR]
    , lookupBlobAuthz = \_ _ -> [pure AllowR]
    , lookupBlobContentAuthz = \_ _ -> [pure AllowR]
    }

-- | True if the object is a boulder
objectIsBoulder :: ObjId -> Avers Bool
objectIsBoulder objId = do
    obj <- lookupObject objId
    return ((objectType obj) == "boulder")

-- | True if the session is an admin.
sessionIsAdmin :: Session -> Avers Bool
sessionIsAdmin session = do
    let sessionId = sessionObjId session
    admins <- runQueryCollect $
            R.Map mapId $
            R.Filter (hasAccess "admin") $
            viewTable accountsView

    elem sessionId <$> (pure $ map ObjId $ V.toList admins)

-- | True if the session is a setter.
sessionIsSetter :: Session -> Avers Bool
sessionIsSetter session = do
    let sessionId = sessionObjId session
    setters <- runQueryCollect $
            R.Map mapId $
            R.Filter (hasAccess "setter") $
            viewTable accountsView

    elem sessionId <$> (pure $ map ObjId $ V.toList setters)

-- | True if the session is in setter list of boulder.
sessionIsBoulderSetter :: Session -> ObjId -> Avers Bool
sessionIsBoulderSetter session objId = do
    let sessionId = sessionObjId session
    setters <- runQueryCollect $
            R.Map mapId $
            R.Filter (hasAccess "setter") $
            viewTable accountsView

    elem sessionId <$> (pure $ map ObjId $ V.toList setters)

