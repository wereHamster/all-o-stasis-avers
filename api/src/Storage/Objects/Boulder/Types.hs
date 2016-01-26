{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Boulder.Types where

import GHC.Generics

import Data.Text
import Data.Time

import Avers
import Avers.TH


-- Date and user automatically in rethinkDB
data Boulder = Boulder
    { boulderSetter     :: [ObjId]
    , boulderSector     :: BoulderSector
    , boulderGrade      :: BoulderGrade
    , boulderGradeNr    :: Int
    , boulderRemoved    :: Maybe UTCTime
    , boulderName       :: Maybe Text
    }
    deriving (Show, Generic)

data BoulderSector
    = SpektrumOne
    | SpektrumTwo
    | SpektrumThree
    | SpektrumFour
    | BigBoss
    | Dune
    | Starship
    | Kurswand
    | Klagemauer
    deriving (Generic)

instance Show BoulderSector where
    show SpektrumOne   = "Spektrum 1"
    show SpektrumTwo   = "Spektrum 2"
    show SpektrumThree = "Spektrum 3"
    show SpektrumFour  = "Spektrum 4"
    show BigBoss       = "Big Boss"
    show Dune          = "Dune"
    show Starship      = "Starship"
    show Kurswand      = "Kurswand"
    show Klagemauer    = "Klagemauer"

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
