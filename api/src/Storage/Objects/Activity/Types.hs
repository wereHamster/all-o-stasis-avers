{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Objects.Activity.Types where

import GHC.Generics

import Data.Text
import Data.Time

import Avers
import Avers.TH

import qualified Storage.Objects.Boulder as B


-- see http://activitystrea.ms/
data Activity = Activity
    { activityActor     :: ObjId
    , activityObject    :: ObjId
    , activityVerb      :: AccountActivity
    , activityDevice    :: String
    } deriving (Show, Generic)

-- FIXME: naming
data AccountActivity
    = DoTick
    | DoComment             Comment
    | DoRate                Rating
    | DoGrade               Grade
    | DoSetProblem
    | DoRemoveProblem
    deriving (Show, Generic)

data Rating
    = Like
    | Dislike
    deriving (Show, Generic)

data Grade = Grade
    { gradeValue :: B.BoulderGrade
    }
    deriving (Show, Generic)

data Comment = Comment
    { commentValue :: Text
    }
    deriving (Show, Generic)

$(deriveEncoding (deriveJSONOptions "activity")     ''Activity)
$(deriveEncoding (deriveJSONOptions "grade")        ''Grade)
$(deriveEncoding (deriveJSONOptions "comment")      ''Comment)
$(deriveEncoding (defaultVariantOptions "")         ''AccountActivity)
$(deriveEncoding (defaultVariantOptions "")         ''Rating)
