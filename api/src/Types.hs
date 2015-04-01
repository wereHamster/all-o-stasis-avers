{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Types where


import Control.Exception (throw)
import Control.Exception (Exception)
import Control.Monad.IO.Class
import Control.Monad.State.Class

import Data.Monoid
import Data.Typeable
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Data.Time
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Monoid


import Snap

import Avers
import Avers.Types (parseObjectId)



data AppState = AppState
    { _aversState :: Avers.Handle
    }

type RequestHandler = Handler AppState AppState


-- | All exceptions which may be safely thrown within a 'RequestHandler'. They
-- are caught by the snaplet and converted into the appropriate response code.

data RequestHandlerException

    = BadRequest !Text
      -- ^ 400 -- The client has submitted a malformed request.

    | Unauthorized
      -- ^ 401 -- Only sent in response to 'POST /session'. It means the
      -- submitted credentials were invalid.

    | Forbidden
      -- ^ 403 -- The request can not be fulfilled because the user does not
      -- have the necessary permissions.

    | NotFound
      -- ^ 404 -- The resource has not been found.

    | InternalServerError !Text
      -- ^ 500 -- Something has gone bad inside the server. It is not the
      -- client's fault. It can try sending the same request again after
      -- a short delay.

    deriving (Show, Typeable)

instance Exception RequestHandlerException


failWith :: RequestHandlerException -> RequestHandler a
failWith = throw


sendErrorResponse :: Int -> Text -> RequestHandler ()
sendErrorResponse status text = do
    liftIO $ print $ "error " <> (T.pack $ show status) <> ": " <> text
    modifyResponse $ setResponseCode status
    writeText $ "{\"error\":\"" <> text <> "\"}"


sendResponse :: (ToJSON a) => a -> RequestHandler ()
sendResponse body = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode body


-- | Run an Avers action within the request handler, using
-- the default Avers configuration.
reqAvers :: Avers a -> RequestHandler (Either AversError a)
reqAvers m = do
    h <- gets _aversState
    liftIO $ evalAvers h m


rhAvers :: Avers a -> RequestHandler a
rhAvers action = reqAvers action >>= aversResult


aversResult :: Either AversError a -> RequestHandler a
aversResult res = case res of
    Left e -> case e of
        DatabaseError detail                  -> failWith $ InternalServerError $ "database " <> detail
        NotAuthorized                         -> failWith $ Unauthorized
        DocumentNotFound _                    -> failWith $ NotFound
        UnknownObjectType detail              -> failWith $ InternalServerError $ "unknown object " <> detail
        ObjectNotFound _                      -> failWith $ NotFound
        ParseError _ detail                   -> failWith $ InternalServerError $ "parse " <> detail
        PatchError (UnknownPatchError detail) -> failWith $ InternalServerError $ "patch " <> detail
        AversError detail                     -> failWith $ InternalServerError $ "avers " <> detail
        InternalError ie                      -> aversResult (Left ie)
    Right r -> return r


replyWith :: Either AversError a -> (a -> RequestHandler ()) -> RequestHandler ()
replyWith res action = aversResult res >>= action


asObjId :: Text -> RequestHandler ObjId
asObjId text = return $ ObjId text

asObjectId :: Text -> RequestHandler ObjectId
asObjectId text = case parseObjectId text of
    Nothing -> failWith $ BadRequest $ "Not an ObjectId: " <> text
    Just v  -> return v


asRevId :: Text -> RequestHandler RevId
asRevId text = return $ RevId (read $ T.unpack text)


type Route = (Method, Text, RequestHandler ())



parseRequestBody :: (FromJSON a) => RequestHandler a
parseRequestBody = do
    body <- readRequestBody $ 10 * 1000 * 1000
    case eitherDecode' body of
        Left e  -> failWith $ BadRequest $ "parseRequestBody: " <> (T.pack $ show e)
        Right x -> return x


textParam :: BS.ByteString -> RequestHandler Text
textParam name = do
    Just value <- getParam name
    return $ E.decodeUtf8 value

optionalTextParam :: BS.ByteString -> RequestHandler (Maybe Text)
optionalTextParam name = do
    mbValue <- getParam name
    return $ fmap E.decodeUtf8 mbValue

