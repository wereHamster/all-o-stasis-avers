{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Boulder
    ( module Storage.Objects.Boulder.Types
    , boulderObjectType
    , bouldersView
    ) where


import Avers
import Control.Monad.State
import Storage.Objects.Boulder.Types

mkObjId :: Int -> Avers ObjId
mkObjId len = ObjId <$> liftIO (newId len)

boulderViews :: [SomeView Boulder]
boulderViews =
    [ SomeView bouldersView
    ]

boulderObjectType :: ObjectType Boulder
boulderObjectType = ObjectType
    { otType   = "boulder"
    , otId     = mkObjId 42
    , otViews  = boulderViews
    }

bouldersView :: View Boulder Boulder
bouldersView = View
    { viewName              = "boulders"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
