{-# LANGUAGE OverloadedStrings #-}

module Queries
    ( mapId
    , isOwnBoulder
    , isSetter
    , hasAccess
    ) where

import           Avers
import           Data.Text          (Text)
import qualified Database.RethinkDB as R


mapId:: R.Exp R.Object -> R.Exp Text
mapId = R.GetField "id"

hasAccess :: R.Exp Text -> R.Exp R.Object -> R.Exp Bool
hasAccess role = \x -> R.All
    [ R.HasFields ["role"] x
    , R.Eq
        (R.GetField "role" x :: R.Exp Text)
        (role :: R.Exp Text)
    ]

-- all non users
isSetter :: R.Exp R.Object -> R.Exp Bool
isSetter = \x -> R.Ne
    (R.GetField "role" x :: R.Exp Text)
    ("user" :: R.Exp Text)

-- FIXME: we should check if the setter is in the list of setters
--        setter is a list of setterIds
isOwnBoulder :: Session -> R.Exp R.Object -> R.Exp Bool
isOwnBoulder session = \x -> R.Eq
    (R.GetField "setter" x :: R.Exp Text)
    (R.lift $ unObjId $ sessionObjId session :: R.Exp Text)
