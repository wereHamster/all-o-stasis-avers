{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Boulder.Types where

import Data.Text
import Data.Time

import Avers
import Avers.TH


data Boulder = Boulder
    { boulderSetter     :: [ObjId]
    , boulderGrade      :: BoulderGrade
    , boulderGradeNr    :: Int
    , boulderDate       :: UTCTime
    , boulderSector     :: Text
    , boulderRemoved    :: Maybe UTCTime
    , boulderName       :: Text
    , boulderComments   :: Text
    }
    deriving (Show)

data BoulderGrade
    = Yellow
    | Green
    | Orange
    | Blue
    | Red
    | White
    deriving (Show)

$(deriveEncoding (deriveJSONOptions "boulder")   ''Boulder)
$(deriveEncoding (defaultVariantOptions "")      ''BoulderGrade)
