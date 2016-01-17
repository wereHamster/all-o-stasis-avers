{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Boulder.Types where

import Data.Text
import Data.Time

import Avers
import Avers.TH


-- FIXME: split in datatypes so that they are editable by everyone with the
-- propper role
data Boulder = Boulder
    { boulderSetter     :: [ObjId]
    , boulderGrade      :: BoulderGrade
    , boulderGradeNr    :: Int
    , boulderSector     :: Text
    , boulderDate       :: UTCTime
    , boulderRemoved    :: Maybe UTCTime
    , boulderName       :: Text
    , boulderComments   :: Text
    , boulderStats      :: BoulderStats
    }

data BoulderStats = BoulderStats
    { boulderLikes      :: Int
    , boulderDislikes   :: Int
    , boulderGradeVotes :: [Int]
    }

data BoulderGrade
    = Yellow
    | Green
    | Orange
    | Blue
    | Red
    | White

$(deriveEncoding (deriveJSONOptions "boulder")   ''Boulder)
$(deriveEncoding (deriveJSONOptions "boulder")   ''BoulderStats)
$(deriveEncoding (defaultVariantOptions "")      ''BoulderGrade)
