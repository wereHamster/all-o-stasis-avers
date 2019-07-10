{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Boulder.Types where

import GHC.Generics

import Data.Text
import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Encoding

import Avers
import Avers.TH


-- Date and creator automatically available in rethinkDB (object)
data Boulder = Boulder
    { boulderSetter     :: [ObjId]
    , boulderSector     :: Text
    , boulderGrade      :: Text
    , boulderGradeNr    :: Int
    , boulderSetDate    :: Int                  -- Data.Time.Calendar.Day
    , boulderRemoved    :: Int                  -- Date.Time.Calendar.Day
    , boulderName       :: Maybe Text
    }
    deriving (Show, Generic)

$(deriveEncoding (deriveJSONOptions "boulder")   ''Boulder)