{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Config.Types
    ( Config(..)
    , defaultConfig
    , pConfig
    , module PassportAuth
    , module Network.URI
    ) where

import Data.Text (Text)
import PassportAuth
import Network.URI
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..))
import Data.Maybe
import Control.Lens
import Configuration.Utils

data Config = Config
    { _cPort :: Int
    , _cAdminAccountEmail :: Text
    , _cRethinkDB :: URI
    , _cPassport :: PassportConfig
    }

$(makeLenses ''Config)

defaultConfig :: Config
defaultConfig = Config
    { _cPort = 8000

    , _cAdminAccountEmail = "admin@boulder.app"

    -- TODO: Find a way to parse the URL at compile time.
    , _cRethinkDB = case parseRelativeReference "//localhost/allostasis" of
        Nothing -> error "defaultConfig"
        Just x -> x

    , _cPassport = PassportConfig
        { _pcRealm = "Boulder App"
        , _pcFrom = "auth@boulder.app"
        , _pcApiDomain = "https://api.boulder.app"
        , _pcAppDomain = "https://boulder.app"
        , _pcSendProvider = PCSPTerminal
        }
    }

pConfig :: MParser Config
pConfig = pure id

instance FromJSON URI where
    parseJSON s = (fromJust . parseRelativeReference) <$> parseJSON s

instance FromJSON (Config -> Config) where
    parseJSON = withObject "Config" $ \o -> id
        <$< cPort ..: "port" × o
        <*< cAdminAccountEmail ..: "adminEmail" % o
        <*< cRethinkDB ..: "rethinkdb" % o
        <*< cPassport %.: "passport" % o

instance ToJSON Config where
    toJSON = undefined
