{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception hiding (Handler, catches)
import           Control.Monad.State
import           Control.Monad.CatchIO hiding (Handler, catch)
import           Control.Monad.Trans.Maybe

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Char8  as C
import           Data.IORef
import           Data.Time
import           Data.Aeson

import           Network.URI

import           System.IO
import           System.Directory
import           System.Environment

import           Snap

import           Avers

import           Types
import           Routes

import           Storage.ObjectTypes
import           Storage.Objects.Boulder.Types

databaseConfig :: IO URI
databaseConfig = do
    uri <- fromMaybe "//localhost/allostasis" <$> lookupEnv "RETHINKDB"
    return $ fromJust $ parseRelativeReference uri


createBlobStorageConfig :: IO (BlobId -> Text -> ByteString -> IO (Either AversError ()))
createBlobStorageConfig = return $ \blobId contentType content ->
    error "Not supported"


enableCors :: RequestHandler ()
enableCors = do
    origin <- getHeader "Origin" <$> getRequest
    case origin of
        Nothing -> return ()
        Just x -> do
            -- CORS (http://www.w3.org/TR/cors/)
            modifyResponse $ setHeader "Access-Control-Allow-Origin"      x
            modifyResponse $ setHeader "Access-Control-Allow-Credentials" "true"
            modifyResponse $ setHeader "Access-Control-Allow-Headers"     "Content-Type, X-Requested-With"
            modifyResponse $ setHeader "Access-Control-Allow-Methods"     "GET, POST, PATCH, DELETE"
            modifyResponse $ setHeader "Access-Control-Max-Age"           "3600"

            -- This is needed so that the browser doens't cache the CORS
            -- headers across origins (see http://www.w3.org/TR/cors/#resource-implementation)
            modifyResponse $ setHeader "Vary" "Origin"

            -- Resource Timing (http://www.w3.org/TR/resource-timing/)
            modifyResponse $ setHeader "Timing-Allow-Origin" x


handleExceptions :: RequestHandler () -> RequestHandler ()
handleExceptions m = m `catches`
    [ -- Handler handleRequestException
    ]


allObjectTypes :: [SomeObjectType]
allObjectTypes =
    [ SomeObjectType accountObjectType
    , SomeObjectType boulderObjectType
    ]


createAversHandle :: IO Avers.Handle
createAversHandle = do
    dbURI <- databaseConfig
    bsc  <- createBlobStorageConfig

    eH <- newState $ Avers.Config dbURI bsc allObjectTypes (\_ _ -> return ())
    h <- case eH of
        Left e -> error $ show e
        Right h -> return h

    res <- evalAvers h Avers.bootstrap
    case res of
        Left e -> error $ show e
        Right _ -> return $ Right ()

    return h

ignoreException :: SomeException -> IO ()
ignoreException e = do
    putStrLn $ "ignoreException: " ++ show e


mainSnaplet :: Avers.Handle -> SnapletInit AppState AppState
mainSnaplet h = makeSnaplet "iff.io" "boulder tracker api" Nothing $ do
    addRoutes $ map toRoute routes

    wrapSite $ \site -> do
        enableCors
        handleExceptions $ method OPTIONS (return ()) <|> site

    return $ AppState h

  where
    toRoute (m, p, action) = (T.encodeUtf8 p, method m action)


main :: IO ()
main = do
    h <- createAversHandle

    -- Start the web server (using custom snap config).
    snapConfig <- do
        let stdioLogger h = ConfigIoLog $ \msg -> BS.hPut h msg >> hFlush h

        config <- commandLineConfig defaultConfig
        return $ setAccessLog (stdioLogger stdout)
               $ setErrorLog (stdioLogger stderr)
               $ config

    serveSnaplet snapConfig $ mainSnaplet h

