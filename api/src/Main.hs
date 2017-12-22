{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Safe

import           Control.Monad.State

import           Data.Text (Text)
import           Data.Maybe
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import qualified Data.Vector as V

import qualified Database.RethinkDB as R

import           Network.URI

import           System.IO
import           System.Environment

import           Avers
import           Avers.API
import           Avers.Server

import           Servant.API
import           Servant.Server

import           Authorization
import           Queries
import           Routes
import           PassportAuth
import           Config

import           Storage.ObjectTypes
import           Storage.Objects.Account

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Cors

import           Prelude



-- Currently we dont need a blob storage (inline avatar images)
createBlobStorageConfig :: IO (BlobId -> Text -> ByteString -> IO (Either AversError ()))
createBlobStorageConfig = return $ \_blobId _contentType _content ->
    error "Not supported"


allObjectTypes :: [SomeObjectType]
allObjectTypes =
    [ SomeObjectType accountObjectType
    , SomeObjectType boulderObjectType
    , SomeObjectType passportObjectType
    ]


createAversHandle :: IO Avers.Handle
createAversHandle = do
    bsc  <- createBlobStorageConfig

    eH <- newHandle $ Avers.Config (cRethinkDB config) bsc allObjectTypes (\_ _ -> return ())
    h <- case eH of
        Left e -> error $ show e
        Right h -> return h

    res <- evalAvers h Avers.bootstrap
    void $ case res of
        Left e -> error $ show e
        Right _ -> return $ Right ()

    return h

bootstrapAdminAccount:: Avers.Handle -> IO ()
bootstrapAdminAccount h = do
    adminAccounts <- evalAvers h $ do
        runQueryCollect $
            R.Filter (hasAccess "admin") $
            viewTable accountsView
    case adminAccounts of
        Left e -> error $ show e
        Right accounts -> if V.length accounts /= 0
            then pure ()
            else do
                res <- evalAvers h createAdminAccount
                void $ case res of
                    Left e -> error $ show e
                    Right _ -> return $ Right ()


type API = AversAPI :<|> LocalAPI

api :: Proxy API
api = Proxy


server :: PassportConfig -> Avers.Handle -> Server API
server pc aversH =
         serveAversAPI aversH Authorization.aosAuthorization
    :<|> serveLocalAPI pc aversH


app :: PassportConfig -> Avers.Handle -> Application
app pc aversH = logStdout $ cors mkCorsPolicy $ serve api $ server pc aversH


mkCorsPolicy :: Request -> Maybe CorsResourcePolicy
mkCorsPolicy req = Just $ simpleCorsResourcePolicy
    { corsOrigins = Just ([origin], True)
    , corsMethods = ["HEAD", "GET", "POST", "PATCH"]
    , corsRequestHeaders = simpleHeaders
    }
  where
    headers = requestHeaders req
    origin = fromMaybe "localhost" $ lookup "Origin" headers


main :: IO ()
main = do
    h <- createAversHandle
    bootstrapAdminAccount h

    run (cPort config) (app (cPassport config) h)
