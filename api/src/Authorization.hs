{-# LANGUAGE OverloadedStrings #-}

module Authorization (aosAuthorization) where

import           Avers                  as Avers
import           Avers.API
import           Avers.Server

import qualified Data.Vector            as V
import qualified Database.RethinkDB     as R

import           Prelude

import           Queries
import           Storage.Objects.Account


aosAuthorization :: Avers.Server.Authorizations
aosAuthorization = Avers.Server.Authorizations
    { createObjectAuthz = \cred objId ->
        [ sufficient $ return (objId == "account")
        , sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            -- we dont need to check (objId == "boulder") for setters for now
            sessionIsAdmin session >> sessionIsSetter session
        , pure RejectR
        ]
    , lookupObjectAuthz = \cred objId ->
        [ sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            sessionIsAdmin session >> sessionCreatedObject session objId
        ]
    , patchObjectAuthz = \cred objId ops ->
        [ sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            sessionIsAdmin session >> sessionIsObject session objId >> sessionCreatedObject session objId
            -- TODO (depends on [operation]):
            --   - only admins can patch the 'role' field
            --   -
        ]
    , deleteObjectAuthz = \_ _ -> [pure RejectR]
    , uploadBlobAuthz = \_ _ -> [pure AllowR]
    , lookupBlobAuthz = \_ _ -> [pure AllowR]
    , lookupBlobContentAuthz = \_ _ -> [pure AllowR]
    }

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

