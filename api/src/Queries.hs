{-# LANGUAGE OverloadedStrings #-}

module Queries
    ( mapId
    , isNotRemoved
    ) where

import           Avers

import           Data.Text   (Text)

import           Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Database.RethinkDB     as R

import           Storage.ObjectTypes


mapId:: R.Exp R.Object -> R.Exp Text
mapId = R.GetField "id"


isNotRemoved :: R.Exp R.Object -> R.Exp Bool
isNotRemoved x = R.Any
    [ R.Not $ R.HasFields ["removed"] x
    , R.Ne
        (R.GetField "removed" x :: R.Exp Text)
        ("" :: R.Exp Text)
    ]

-- FIXME: we should check if the setter is in the list of setters
--        setter is a list of setterIds
isOwnBoulder :: Session -> R.Exp R.Object -> R.Exp Bool
isOwnBoulder session = \x -> R.Eq
    (R.GetField "setter" x :: R.Exp Text)
    (R.lift $ unObjId $ sessionObjId session)

