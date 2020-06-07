{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Boulder
    ( module Storage.Objects.Boulder.Types
    , boulderObjectType
    , bouldersView
    , activeBouldersView
    ) where


import Avers
import Control.Monad.State
import Storage.Objects.Boulder.Types

mkObjId :: Int -> Avers ObjId
mkObjId len = ObjId <$> liftIO (newId len)

boulderViews :: [SomeView Boulder]
boulderViews =
    [ SomeView bouldersView
    , SomeView activeBouldersView
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

activeBouldersView :: View Boulder Boulder
activeBouldersView = View
    { viewName              = "activeBoulders"
    , viewParser            = parseDatum
    , viewObjectTransformer = \boulder ->
        if boulderRemoved boulder > 0 then pure Nothing else pure (Just boulder)
    , viewIndices           = []
    }
