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

import           Storage.ObjectTypes
import           Storage.Objects.Account

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Cors

import           Prelude



databaseConfig :: IO URI
databaseConfig = do
    uri <- fromMaybe "//localhost/allostasis" <$> lookupEnv "RETHINKDB"
    return $ fromJust $ parseRelativeReference uri


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
    dbURI <- databaseConfig
    bsc  <- createBlobStorageConfig

    eH <- newHandle $ Avers.Config dbURI bsc allObjectTypes (\_ _ -> return ())
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


server :: Avers.Handle -> Server API
server aversH =
         serveAversAPI aversH Authorization.aosAuthorization
    :<|> serveLocalAPI aversH


app :: Avers.Handle -> Application
app aversH = logStdout $ cors mkCorsPolicy $ serve api $ server aversH


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
    args <- getArgs
    h <- createAversHandle
    bootstrapAdminAccount h

    mbPort <- pure $ case args of
        x:_ -> readMay x
        _   -> Nothing

    run (fromMaybe 8000 mbPort) (app h)
