{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Monad.State

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import qualified Data.Vector as V

import qualified Database.RethinkDB as R

import           System.IO

import           Avers hiding (Config)
import qualified Avers
import           Avers.API
import           Avers.Server

import           Servant.API
import           Servant.Server

import           Authorization
import           Queries
import           Routes
import           Config

import           Storage.ObjectTypes
import           Storage.Objects.Account

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Cors

import           Configuration.Utils

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


createAversHandle :: Config -> IO Avers.Handle
createAversHandle config = do
    bsc  <- createBlobStorageConfig

    dbURI <- case parseRelativeReference (T.unpack $ _cRethinkDB config) of
        Nothing -> error "createAversHandle: bad RethinkDB URI"
        Just uri -> pure uri

    eH <- newHandle $ Avers.Config dbURI bsc allObjectTypes (\_ _ -> return ())
    h <- case eH of
        Left e -> error $ show e
        Right h -> return h

    res <- evalAvers h Avers.bootstrap
    void $ case res of
        Left e -> error $ show e
        Right _ -> return $ Right ()

    return h

bootstrapAdminAccount:: Config -> Avers.Handle -> IO ()
bootstrapAdminAccount config h = do
    adminAccounts <- evalAvers h $ do
        runQueryCollect $
            R.Filter (hasAccess "admin") $
            viewTable accountsView
    case adminAccounts of
        Left e -> error $ show e
        Right accounts -> if V.length accounts /= 0
            then pure ()
            else do
                res <- evalAvers h (createAdminAccount config)
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

mainInfo :: ProgramInfo Config
mainInfo = programInfo "Config" pConfig defaultConfig

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
    h <- createAversHandle config
    bootstrapAdminAccount config h

    run (_cPort config) (app (_cPassport config) h)
