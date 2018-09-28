{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Config
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
import Control.Lens
import Configuration.Utils

data Config = Config
    { _cPort :: Int
    , _cAdminAccountEmail :: Text
    , _cRethinkDB :: Text
    , _cPassport :: PassportConfig
    }

$(makeLenses ''Config)

defaultConfig :: Config
defaultConfig = Config
    { _cPort = 8000

    , _cAdminAccountEmail = "admin@boulder.app"

    , _cRethinkDB = "//localhost/allostasis"

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

instance FromJSON (Config -> Config) where
    parseJSON = withObject "Config" $ \o -> id
        <$< cPort ..: "port" × o
        <*< cAdminAccountEmail ..: "adminEmail" % o
        <*< cRethinkDB ..: "rethinkdb" % o
        <*< cPassport %.: "passport" % o

instance ToJSON Config where
    toJSON = undefined
