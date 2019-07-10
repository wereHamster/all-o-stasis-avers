{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}

module Routes
    ( PassportConfig(..)
    , LocalAPI
    , serveLocalAPI
    ) where

import Control.Monad.Except
import Control.Concurrent

import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Data.Monoid

import Data.Aeson

import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT

import qualified Data.Vector as V

import           Data.Map (Map)
import qualified Data.Map as M

import qualified Database.RethinkDB as R

import           Network.Mail.Mime (Mail(..), Part(..), Address(..))
import           Network.HTTP.Simple

import Avers as Avers
import Avers.TH
import Avers.API
import Avers.Server

import Servant.API hiding (Patch)
import Servant.Server

import Web.Cookie
import Network.Gravatar

import Queries
import Revision
import Types
import Wordlist
import PassportAuth
import PassportConfirmationEmail

import Storage.ObjectTypes
import Storage.Objects.Account
import Storage.Objects.Boulder
import Storage.Objects.Passport

import Prelude


data SignupRequest2 = SignupRequest2
    { reqLogin     :: Text
    }

data SignupResponse2 = SignupResponse2
    { _resObjId :: ObjId
    }



-------------------------------------------------------------------------------
type CreatePassport
    = "login"
        :> ReqBody '[JSON] CreatePassportBody
        :> Post '[JSON] CreatePassportResponse

data CreatePassportBody = CreatePassportBody
    { reqEmail :: Text
    }

data CreatePassportResponse = CreatePassportResponse
    { _resPassportId :: Text
    , _resSecurityCode :: Text
    }


-------------------------------------------------------------------------------
type ConfirmPassport
    = "login" :> "confirm"
        :> QueryParam "passportId" Text
        :> QueryParam "confirmationToken" Text
        :> Get '[JSON] (Headers '[Header "Location" Text] NoContent)


-------------------------------------------------------------------------------
type AwaitPassportConfirmation
    = "login" :> "verify"
        :> QueryParam "passportId" Text
        :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)


-------------------------------------------------------------------------------
type PassportAPI
    =    CreatePassport
    :<|> ConfirmPassport
    :<|> AwaitPassportConfirmation


type SetterMonthlyStats = Map Text Int

data BoulderStat = BoulderStat
    { bsSetOn :: Day
    , bsRemovedOn :: Maybe Day
    , bsSetters :: [ObjId]
    , bsSector :: Text
    , bsGrade :: Text
    }

data PublicProfile = PublicProfile
    { ppName :: Text
    , ppAvatar :: Maybe Text
    }

type LocalAPI
    -- server the git revsion sha
    = "revision"
      :> Get '[PlainText] Text

    -- health check endpoint
    :<|> "healthz"
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
      :> ReqBody '[JSON] SignupRequest2
      :> Post '[JSON] SignupResponse2

    :<|> "stats"
        :> Capture "setterId" ObjId
        :> Capture "year" Integer
        :> Capture "month" Int -- 1..12
        :> Get '[JSON] SetterMonthlyStats

    :<|> "stats" :> "boulders"
        :> Get '[JSON] [BoulderStat]

    :<|> "public-profile"
        :> Capture "accountId" ObjId
        :> Get '[JSON] PublicProfile

    :<|> PassportAPI


serveLocalAPI :: PassportConfig -> Avers.Handle -> Server LocalAPI
serveLocalAPI pc aversH =
         serveRevision
    :<|> serveHealthz
    :<|> serveActiveBouldersCollection
    :<|> serveOwnBouldersCollection
    :<|> serveAccounts
    :<|> serveAdminAccounts
    :<|> serveSignup
    :<|> serveSetterMonthlyStats
    :<|> serveBouldersStats
    :<|> servePublicProfile
    :<|> servePassportAPI

  where
    servePassportAPI =
             serveCreatePassport
        :<|> serveConfirmPassport
        :<|> serveAwaitPassportConfirmation

    ----------------------------------------------------------------------------
    sessionCookieName     = "session"
    sessionExpirationTime = 2 * 365 * 24 * 60 * 60

    mkSetCookie :: SessionId -> Handler SetCookie
    mkSetCookie sId = do
        now <- liftIO $ getCurrentTime
        pure $ def
            { setCookieName = sessionCookieName
            , setCookieValue = T.encodeUtf8 (unSessionId sId)
            , setCookiePath = Just "/"
            , setCookieExpires = Just $ addUTCTime sessionExpirationTime now
            , setCookieHttpOnly = True
            }


    serveRevision =
        pure $ T.pack $ fromMaybe "HEAD" $(revision)

    serveHealthz = do
        -- Run a really simple query to check that the database is also alive.
        void $ reqAvers2 aversH $ runQueryCollect $
            R.Filter (hasAccess "admin") $
            viewTable accountsView

        pure "ok"

    serveActiveBouldersCollection = do
        boulders <- reqAvers2 aversH $ do
            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "setDate"] $
                viewTable activeBouldersView

        pure $ map ObjId $ V.toList boulders

    serveOwnBouldersCollection cred = do
        ownerId <- credentialsObjId aversH cred
        objIds <- reqAvers2 aversH $ do
            -- FIXME: we should check if the setter is in the list of setters
            let isOwnBoulderA :: R.Exp R.Object -> R.Exp Bool
                isOwnBoulderA = \x -> R.Eq
                    (R.GetField "setter" x :: R.Exp Text)
                    (R.lift $ unObjId ownerId)

            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "setDate"] $
                R.Filter isOwnBoulderA $
                viewTable bouldersView

        pure $ map ObjId $ V.toList objIds

    serveAccounts = do
        objIds <- reqAvers2 aversH $ do
            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "name"] $
                viewTable accountsView

        pure $ map ObjId $ V.toList objIds

    serveAdminAccounts _cred = do
        -- ownerId <- credentialsObjId aversH cred
        objIds <- reqAvers2 aversH $ do
            runQueryCollect $
                R.Map mapId $
                R.OrderBy [R.Descending "name"] $
                R.Filter isSetter $
                viewTable accountsView

        pure $ map ObjId $ V.toList objIds


    createAccount :: Text -> Maybe Text -> Handler ObjId
    createAccount login mbEmail = do
        reqAvers2 aversH $ do
            accId <- Avers.createObject accountObjectType rootObjId $ Account
                { accountLogin = login
                , accountRole = User
                , accountEmail = mbEmail
                , accountName = Just ""
                }

            -- TODO: Is this necessary if we have login only via email? It's rather
            -- dangerous to have accounts protected with an empty secret because then
            -- anyone can authenticate against that account.
            updateSecret (SecretId (unObjId accId)) ""

            pure accId

    serveSignup body = do
        accId <- createAccount (reqLogin body) Nothing
        pure $ SignupResponse2 accId


    serveSetterMonthlyStats setterId year month = do
        -- Fetch all boulders (this may be inefficient when we'll have many boulders)
        allBoulders <- reqAvers2 aversH $ do
            datums <- runQueryCollect $ viewTable bouldersView
            V.sequence $ V.map parseDatum datums

        -- Filter those which match the setter, year, and month.
        let boulders = V.filter (\Boulder{..} ->
                let utcTime = posixSecondsToUTCTime (fromIntegral boulderSetDate / 1000)
                    (y, m, _) = toGregorian (utctDay utcTime)
                in y == year && m == month && setterId `elem` boulderSetter
                ) allBoulders

        -- And construct the response.
        let entries = map (\Boulder{..} -> (boulderGrade, 1)) $ V.toList boulders
        pure $ foldl (\m (grade, count) -> M.insertWith (+) grade count m) M.empty entries


    serveBouldersStats = do
        allBoulders <- reqAvers2 aversH $ do
            datums <- runQueryCollect $ viewTable bouldersView
            V.sequence $ V.map parseDatum datums

        let toBoulderStat Boulder{..} = BoulderStat
                { bsSetOn = toUTCTime boulderSetDate
                , bsRemovedOn = if boulderRemoved == 0
                    then Nothing
                    else Just (toUTCTime boulderRemoved)
                , bsSetters = boulderSetter
                , bsSector = boulderSector
                , bsGrade = boulderGrade
                }
              where
                toUTCTime x = utctDay (posixSecondsToUTCTime (fromIntegral x / 1000))

        pure $ map toBoulderStat $ V.toList allBoulders

    servePublicProfile objId = do
        Account{..} <- reqAvers2 aversH $ do
            Snapshot{..} <- lookupLatestSnapshot (BaseObjectId objId)
            case parseValueAs accountObjectType snapshotContent of
                Left e  -> throwError e
                Right x -> pure x

        pure $ PublicProfile
            { ppName = fromMaybe (unObjId objId) accountName
            , ppAvatar = case accountEmail of
                Nothing -> Nothing
                Just email -> Just $ T.pack $ gravatar def email
            }

    serveCreatePassport CreatePassportBody{..} = do
        -- 1. Lookup account by email. If no such account exists, create a new one.
        accId <- do
            accountIds <- reqAvers2 aversH $ runQueryCollect $
                R.Limit 1 $
                R.Map mapId $
                R.Filter (matchEmail (R.lift reqEmail)) $
                viewTable accountsView

            case V.toList accountIds of
                [accId] -> pure $ ObjId accId
                _ -> do
                    liftIO $ putStrLn $ T.unpack $ "Account with email " <>
                        reqEmail <> " not found, creating new account."
                    createAccount reqEmail (Just reqEmail)

        -- 2. Create a new Passport object.
        securityCode <- liftIO mkSecurityCode
        confirmationToken <- liftIO (newId 16)

        passportId <- reqAvers2 aversH $ do
            Avers.createObject passportObjectType rootObjId $ Passport
                { passportAccountId = accId
                , passportSecurityCode = securityCode
                , passportConfirmationToken = confirmationToken
                , passportValidity = PVUnconfirmed
                }

        -- 3. Send email
        -- TODO: actually send the email.
        -- TODO: link requires the full domain name where the API is hosted,
        -- it therefore must be configurable.
        let mail = passportConfirmationEmail
                pc reqEmail (unObjId passportId) securityCode confirmationToken
        case _pcSendProvider pc of
            PCSPTerminal -> liftIO $ do
                putStrLn "\n\n-------------------------"
                putStrLn $ show mail
                putStrLn "\n\n-------------------------"

            PCSPSendgrid apiKey -> liftIO $ do
                let partToContent :: Part -> Value
                    partToContent part = object
                        [ "type" .= ("text/plain" :: Text) -- partType part
                        , "value" .= LT.decodeUtf8 (partContent part)
                        ]

                let subject = fromMaybe "???" $ lookup "Subject" (mailHeaders mail)

                let toPersonalization addr = object
                        [ "to" .= [ object [ "email" .= addressEmail addr ] ]
                        , "subject" .= subject
                        ]

                print $ mailParts mail
                let body = object
                        [ "personalizations" .= map toPersonalization (mailTo mail)
                        , "from" .= object
                            [ "email" .= addressEmail (mailFrom mail)
                            ]
                        , "content" .= concatMap (map partToContent) (mailParts mail)
                        ]

                let request = setRequestBodyJSON body
                        $ setRequestHeader "Content-Type" ["application/json"]
                        $ setRequestHeader "Authorization" ["Bearer " <> T.encodeUtf8 apiKey]
                        $ "POST https://api.sendgrid.com/v3/mail/send"

                response <- httpLBS request
                print response



        -- 4. Send response
        pure $ CreatePassportResponse
            { _resPassportId = unObjId passportId
            , _resSecurityCode = securityCode
            }

    serveConfirmPassport mbPassportId mbConfirmationToken = do
        -- Query params in Servant are always optional (Maybe), but we require them here.
        passportId <- case mbPassportId of
            Nothing -> throwError err400 { errBody = "passportId missing" }
            Just pId -> pure $ ObjId pId

        confirmationToken <- case mbConfirmationToken of
            Nothing -> throwError err400 { errBody = "confirmationToken missing" }
            Just x -> pure x

        -- Lookup the latest snapshot of the Passport object.
        (Snapshot{..}, Passport{..}) <- reqAvers2 aversH $ do
            snapshot <- lookupLatestSnapshot (BaseObjectId passportId)
            passport <- case parseValueAs passportObjectType (snapshotContent snapshot) of
                Left e  -> throwError e
                Right x -> pure x

            pure (snapshot, passport)

        -- Check the confirmationToken. Fail if it doesn't match.
        when (confirmationToken /= passportConfirmationToken) $ do
            throwError err400 { errBody = "wrong confirmation token" }

        -- Patch the "validity" field to mark the Passport as valid.
        void $ reqAvers2 aversH $ applyObjectUpdates
            (BaseObjectId passportId)
            snapshotRevisionId
            rootObjId
            [Set { opPath = "validity", opValue = Just (toJSON PVValid) }]
            False

        -- Apparently this is how you do a 30x redirect in Servant…
        throwError $ err301
            { errHeaders = [("Location", T.encodeUtf8 (_pcAppDomain pc) <> "/email-confirmed")]
            }

    -- This request blocks until the Passport either becomes valid or expires.
    serveAwaitPassportConfirmation mbPassportId = do
        passportId <- case mbPassportId of
            Nothing -> throwError err400
            Just pId -> pure $ ObjId pId

        let go = do
                -- Lookup the latest snapshot of the Passport object.
                (Snapshot{..}, Passport{..}) <- reqAvers2 aversH $ do
                    snapshot <- lookupLatestSnapshot (BaseObjectId passportId)
                    passport <- case parseValueAs passportObjectType (snapshotContent snapshot) of
                        Left e  -> throwError e
                        Right x -> pure x

                    pure (snapshot, passport)

                case passportValidity of
                    PVValid ->
                        -- Exit the loop.
                        pure (passportAccountId, snapshotRevisionId)

                    PVUnconfirmed ->
                        -- Sleep a bit and then retry.
                        liftIO (threadDelay 500000) >> go

                    PVExpired ->
                        -- Fail the request.
                        throwError err400

        (accId, revId) <- go

        -- Mark the passport as expired, so that it can not be reused.
        void $ reqAvers2 aversH $ applyObjectUpdates
            (BaseObjectId passportId)
            revId
            rootObjId
            [Set { opPath = "validity", opValue = Just (toJSON PVExpired) }]
            False

        -- The Passport object is valid. Create a new session for the
        -- account in the Passport object.
        now <- liftIO getCurrentTime
        sessId <- SessionId <$> liftIO (newId 80)

        reqAvers2 aversH $ saveSession $ Session sessId accId now now

        setCookie <- mkSetCookie sessId

        -- 4. Respond with the session cookie and status=200
        pure $ addHeader setCookie NoContent


$(deriveJSON (deriveJSONOptions "req")  ''SignupRequest2)
$(deriveJSON (deriveJSONOptions "_res") ''SignupResponse2)

$(deriveJSON (deriveJSONOptions "req")  ''CreatePassportBody)
$(deriveJSON (deriveJSONOptions "_res") ''CreatePassportResponse)

$(deriveJSON (deriveJSONOptions "bs") ''BoulderStat)
$(deriveJSON (deriveJSONOptions "pp") ''PublicProfile)
