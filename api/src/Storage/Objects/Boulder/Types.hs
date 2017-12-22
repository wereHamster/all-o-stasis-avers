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
    , boulderSector     :: BoulderSector
    , boulderGrade      :: BoulderGrade
    , boulderGradeNr    :: Int
    , boulderSetDate    :: Int                  -- Data.Time.Calendar.Day
    , boulderRemoved    :: Int                  -- Date.Time.Calendar.Day
    , boulderName       :: Maybe Text
    }
    deriving (Show, Generic)

data BoulderSector
    = Spektrumone
    | Spektrumtwo
    | Spektrumthree
    | Spektrumfour
    | Bigboss
    | Dune
    | Starship
    | Kurswand
    | Klagemauer
    deriving (Generic)

-- FIXME should be configurable
instance Show BoulderSector where
    show Spektrumone   = "Spektrum 1"
    show Spektrumtwo   = "Spektrum 2"
    show Spektrumthree = "Spektrum 3"
    show Spektrumfour  = "Spektrum 4"
    show Bigboss       = "Big Boss"
    show Dune          = "Dune"
    show Starship      = "Starship"
    show Kurswand      = "Kurswand"
    show Klagemauer    = "Klagemauer"

-- FIXME should be configurable
data BoulderGrade
    = Yellow
    | Green
    | Orange
    | Blue
    | Red
    | White
    deriving (Show, Eq, Ord, Generic)

$(deriveEncoding (deriveJSONOptions "boulder")   ''Boulder)
$(deriveEncoding (defaultVariantOptions "")      ''BoulderGrade)
$(deriveEncoding (defaultVariantOptions "")      ''BoulderSector)

instance ToJSONKey BoulderGrade where
    toJSONKey = ToJSONKeyText f g
        where f = T.pack . show
              g = text . T.pack . show
