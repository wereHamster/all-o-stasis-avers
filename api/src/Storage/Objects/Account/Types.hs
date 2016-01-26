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
    = User
    | Setter
    | Admin
    deriving (Show, Generic)

$(deriveEncoding (deriveJSONOptions "account") ''Account)
$(deriveEncoding (defaultVariantOptions "")    ''AccountRole)
