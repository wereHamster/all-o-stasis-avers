{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Account.Types where

import GHC.Generics

import Data.Text (Text)
import Avers.TH


-- FIXME: proper salt?
data Account = Account
    { accountLogin  :: Text
    , accountRole   :: AccountRole
    , accountEmail  :: Maybe Text
    , accountName   :: Maybe Text
    } deriving (Show, Generic)

-- Available ACL groups
data AccountRole
    = User                          -- view only
    | Setter                        -- modify own boulder
    | Admin                         -- modify boulders and setters
    deriving (Show, Generic)

$(deriveEncoding (deriveJSONOptions "account") ''Account)
$(deriveEncoding (defaultVariantOptions "")    ''AccountRole)
