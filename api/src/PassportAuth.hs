module PassportAuth
    ( PassportConfig(..)
    ) where

import Data.Text (Text)


data PassportConfig = PassportConfig
    { pcRealm :: Text -- The app name (eg. "Minimum Boulder App")
    , pcApiDomain :: Text -- The domain where the API runs (eg. "https://api.app.com")
    , pcAppDomain :: Text -- The domain where the app runs (eg. 'https://app.com')
    }
