{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Activity
    ( module Storage.Objects.Activity.Types
    , activitiesView
    ) where

import Avers
import Control.Monad.State
import Control.Applicative
import Storage.Objects.Activity.Types


views :: [SomeView Activity]
views =
    [ SomeView activitiesView
    ]

activitiesView :: View Activity Activity
activitiesView = View
    { viewName              = "activity"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
