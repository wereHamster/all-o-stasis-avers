module Config.Types
    ( Config(..)
    , module PassportAuth
    , module Network.URI
    ) where

import Data.Text (Text)
import PassportAuth
import Network.URI

data Config = Config
    { cPort :: Int
    , cAdminAccountEmail :: Text
    , cRethinkDB :: URI
    , cPassport :: PassportConfig
    }
