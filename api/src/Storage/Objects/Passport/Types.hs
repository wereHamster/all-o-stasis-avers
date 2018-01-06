{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Passport.Types where

import GHC.Generics

import Data.Text

import Avers
import Avers.TH


data Passport = Passport
    { passportAccountId :: ObjId
    , passportSecurityCode :: Text
    , passportConfirmationToken :: Text
    , passportValidity :: PassportValidity
    } deriving (Show, Generic)

data PassportValidity
    = PVUnconfirmed
    | PVValid
    | PVExpired
    deriving (Show, Generic)


$(deriveEncoding (deriveJSONOptions "passport") ''Passport)
$(deriveEncoding (defaultVariantOptions "PV")   ''PassportValidity)
