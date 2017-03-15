{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Boulder.Types where

import GHC.Generics

import Data.Text

import Avers
import Avers.TH


-- Date and creator automatically available in rethinkDB (object)
data Boulder = Boulder
    { boulderSetter     :: [ObjId]
    , boulderSector     :: BoulderSector
    , boulderGrade      :: BoulderGrade
    , boulderGradeNr    :: Int
    , boulderSetDate    :: Int                  -- Data.Time.Calendar.Day
    , boulderRemoved    :: Maybe Int            -- Date.Time.Calendar.Day
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
    deriving (Show, Generic)

$(deriveEncoding (deriveJSONOptions "boulder")   ''Boulder)
$(deriveEncoding (defaultVariantOptions "")      ''BoulderGrade)
$(deriveEncoding (defaultVariantOptions "")      ''BoulderSector)
