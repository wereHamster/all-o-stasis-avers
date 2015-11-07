{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Account.Types where

import Data.Text (Text)
import Avers.TH


data Account = Account
    { accountLogin :: Text
    , accountEmail :: Text
    , accountName  :: Text
    , accountRole  :: AccountRole
    } deriving (Show)

data AccountRole
    = User
    | Setter
    | Admin
    deriving (Show)

$(deriveEncoding (deriveJSONOptions "account") ''Account)
$(deriveEncoding (defaultVariantOptions "")    ''AccountRole)
