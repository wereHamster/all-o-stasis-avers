{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module PassportAuth
    ( PassportConfig(..)
    , PCSendProvider(..)
    ) where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Control.Lens
import Configuration.Utils


data PCSendProvider
    = PCSPTerminal
    | PCSPSendgrid Text -- Sendgrid API Key

instance FromJSON PCSendProvider where
    parseJSON (Y.Object v) = do
        kind <- v .: "kind"
        case kind :: Text of
            "terminal" -> pure PCSPTerminal
            "sendgrid" -> PCSPSendgrid <$> v .: "apiKey"
            _ -> fail "PCSendProvider: unknown kind"

    parseJSON _ = fail "Expected Object for PCSendProvider value"


data PassportConfig = PassportConfig
    { _pcRealm :: Text -- The app name (eg. "Minimum Boulder App")
    , _pcFrom :: Text -- The From: email address
    , _pcApiDomain :: Text -- The domain where the API runs (eg. "https://api.app.com")
    , _pcAppDomain :: Text -- The domain where the app runs (eg. 'https://app.com')
    , _pcSendProvider :: PCSendProvider
    }

$(makeLenses ''PassportConfig)

instance FromJSON (PassportConfig -> PassportConfig) where
    parseJSON = withObject "PassportConfig" $ \o -> id
        <$< pcRealm ..: "realm" × o
        <*< pcFrom ..: "from" % o
        <*< pcApiDomain ..: "apiDomain" % o
        <*< pcAppDomain ..: "appDomain" % o
        <*< pcSendProvider ..: "sendProvider" % o
