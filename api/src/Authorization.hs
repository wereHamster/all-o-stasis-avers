{-# LANGUAGE OverloadedStrings #-}

module Authorization where


import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Except

import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import qualified Data.Vector as V

import           Avers as Avers

import           Prelude

import           Queries
import           Storage.Objects.Account

import qualified Database.RethinkDB     as R


-- | The result of an authorization query. The combinators are modelled losely
-- after PAM (with control flags)
data AuthzResult a
    = Continue a
      -- ^ The result is undecided. We con continue executing the following
      -- instructions.

    | Fail a
      -- ^ The result will be ultimately be Reject, but we continue executing
      -- following instructions.

    | Allow
      -- ^ The authorization query definitely succeeded. Any following
      -- instructions are skipped.

    | Reject
      -- ^ The authorization query definitely failed. Any following
      -- instructions are skipped.


data Authz a = Authz { runAuthz :: Avers (AuthzResult a) }

instance Functor Authz where
    fmap f m = Authz $ do
        res <- runAuthz m
        case res of
            Continue a -> return $ Continue $ f a
            Fail     a -> return $ Fail $ f a
            Allow      -> return Allow
            Reject     -> return Reject

instance Applicative Authz where
    pure = Authz . pure . Continue
    mf <*> m = Authz $ do
        fres <- runAuthz mf
        mres <- runAuthz m
        case (fres, mres) of
            (Continue f, Continue res) -> return $ Continue $ f res
            _                          -> return Reject

instance Monad Authz where
    return = Authz . return . Continue

    (Authz m) >>= f = Authz $ do
        res <- m
        case res of
            Continue a -> runAuthz $ f a
            Fail     a -> runAuthz $ f a
            Allow      -> return Allow
            Reject     -> return Reject


-- | An authorization module is an Avers action which returns a boolean
-- indicating whether it has succeeded or failed. Exceptions are treated
-- as failure.
type AuthzModule = Avers Bool


sufficient :: AuthzModule -> Authz ()
sufficient m = Authz $ do
    res <- m
    return $ if res then Allow else Continue ()


requisite :: AuthzModule -> Authz ()
requisite m = Authz $ do
    res <- m
    return $ if not res then Reject else Continue ()


required :: AuthzModule -> Authz ()
required m = Authz $ do
    res <- m
    return $ if not res then Fail () else Continue ()



runAuthorization :: Avers () -> Authz a -> Avers ()
runAuthorization def authz = do
    res <- runAuthz authz
    case res of
        Allow  -> return ()
        Reject -> throwError NotAuthorized
        Fail _ -> throwError NotAuthorized
        _      -> def


-- | Only setters can create boulders, admins can create any objects.
authorizeObjectCreate :: ObjId -> Text -> Avers ()
authorizeObjectCreate sessionId objType = runAuthorization (throwError NotAuthorized) $ do
    sufficient $ sessionIsAdmin sessionId
    -- sufficient $ sessionIsSetter sessionId (objType == "boulder")
    -- sufficient $ return


-- | Anyone can create new blobs.
authorizeBlobCreate :: ObjId -> Avers ()
authorizeBlobCreate _ = return ()


-- | Only admins can delete (and undelete) objects.
authorizeObjectDelete :: ObjId -> ObjId -> Avers ()
authorizeObjectDelete sessionId _objId = runAuthorization (throwError NotAuthorized) $ do
    sufficient $ sessionIsAdmin sessionId


-- | Admins can patch anything. The owner of an object can patch the object.
authorizePatch :: ObjId -> ObjId -> Avers ()
authorizePatch sessionId objId = runAuthorization (throwError NotAuthorized) $ do
    sufficient $ sessionIsAdmin sessionId
    sufficient $ sessionIsObject sessionId objId
    sufficient $ sessionCreatedObject sessionId objId



------------------------------------------------------------------------------
-- Authorization modules
------------------------------------------------------------------------------

-- | True if the session is an admin.
sessionIsAdmin :: ObjId -> AuthzModule
sessionIsAdmin sessionId = do
    admins <- runQueryCollect $
            R.Map mapId $
            R.Filter Queries.hasAdminAccessRights $
            viewTable accountsView

    elem sessionId <$> (pure $ map ObjId $ V.toList admins)

-- | True if the session is an setter.
sessionIsSetter :: ObjId -> AuthzModule
sessionIsSetter sessionId = do
    setter <- runQueryCollect $
            R.Map mapId $
            R.Filter Queries.hasSetterAccessRights $
            viewTable accountsView

    elem sessionId <$> (pure $ map ObjId $ V.toList setter)

-- | True if the session created the given object.
sessionCreatedObject :: ObjId -> ObjId -> AuthzModule
sessionCreatedObject sessionId objId = do
    obj <- Avers.lookupObject objId
    return $ objectCreatedBy obj == sessionId

-- | True if the session is the given object.
sessionIsObject :: ObjId -> ObjId -> AuthzModule
sessionIsObject sessionId objId = do
    return $ sessionId == objId

