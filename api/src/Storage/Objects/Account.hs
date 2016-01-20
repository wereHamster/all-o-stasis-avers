{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Account
    ( module Storage.Objects.Account.Types
    , accountsView
    ) where

import Avers
import Control.Monad.State
import Storage.Objects.Account.Types


views :: [SomeView Account]
views =
    [ SomeView accountsView
    ]

accountsView :: View Account Account
accountsView = View
    { viewName              = "accounts"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
