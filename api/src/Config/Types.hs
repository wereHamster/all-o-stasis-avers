module Config.Types
    ( Config(..)
    , module PassportAuth
    , module Network.URI
    ) where

import PassportAuth
import Network.URI

data Config = Config
    { cPort :: Int
    , cRethinkDB :: URI
    , cPassport :: PassportConfig
    }
