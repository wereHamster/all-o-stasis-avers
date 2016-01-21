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
    } deriving (Show)


data AccountActivity
    = AcTick
    | AcRating              Rating
    | AcGrade               Grade
    | AcSetProblem
    | AcRemoveProblem
    | AcUpgradeProblem      Grade
    | AcDowngradeProblem    Grade
    deriving (Show)

data Rating
    = Like
    | Dislike
    deriving (Show)

data Grade = Grade
    { gradeValue :: [B.BoulderGrade]
    }
    deriving (Show)

$(deriveEncoding (deriveJSONOptions "activity")     ''Activity)
$(deriveEncoding (deriveJSONOptions "grade")        ''Grade)
$(deriveEncoding (defaultVariantOptions "Ac")       ''AccountActivity)
$(deriveEncoding (defaultVariantOptions "")         ''Rating)
