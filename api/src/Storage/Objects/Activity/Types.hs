{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Activity.Types where

import Data.Text
import Data.Time

import Avers
import Avers.TH

import qualified Storage.Objects.Boulder as B


data Activity = Activity
    { activityAccount   :: ObjId
    , activityBoulder   :: ObjId
    , activityTimestamp :: UTCTime
    , activityAction    :: AccountActivity
    , activityGrade     :: Maybe B.BoulderGrade -- FIXME: redesign
    } deriving (Show)


data AccountActivity
    = Like
    | Dislike
    | Tick
    | Grade
    | Climb
    | SetProblem
    | RemoveProblem
    | UpgradeProblem
    | DowngradeProblem
    deriving (Show)

$(deriveEncoding (deriveJSONOptions "activity")     ''Activity)
$(deriveEncoding (defaultVariantOptions "")         ''AccountActivity)
