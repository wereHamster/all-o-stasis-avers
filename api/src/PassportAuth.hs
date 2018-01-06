module PassportAuth
    ( PassportConfig(..)
    , PCSendProvider(..)
    ) where

import Data.Text (Text)


data PassportConfig = PassportConfig
    { pcRealm :: Text -- The app name (eg. "Minimum Boulder App")
    , pcFrom :: Text -- The From: email address
    , pcApiDomain :: Text -- The domain where the API runs (eg. "https://api.app.com")
    , pcAppDomain :: Text -- The domain where the app runs (eg. 'https://app.com')
    , pcSendProvider :: PCSendProvider
    }

data PCSendProvider
    = PCSPTerminal
    | PCSPSendgrid Text -- Sendgrid API Key
