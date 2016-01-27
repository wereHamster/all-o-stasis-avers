{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Activity
    ( module Storage.Objects.Activity.Types
    , activityObjectType
    , activitiesView
    ) where

import Avers
import Control.Monad.State
import Storage.Objects.Activity.Types

mkObjId :: Int -> Avers ObjId
mkObjId len = ObjId <$> liftIO (newId len)

activityViews :: [SomeView Activity]
activityViews =
    [ SomeView activitiesView
    ]

activityObjectType :: ObjectType Activity
activityObjectType = ObjectType
    { otType   = "boulder"
    , otId     = mkObjId 42
    , otViews  = activityViews
    }

activitiesView :: View Activity Activity
activitiesView = View
    { viewName              = "activity"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
