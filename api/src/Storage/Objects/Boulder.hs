{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Boulder
    ( module Storage.Objects.Boulder.Types
    , boulderObjectType
    , bouldersView
    ) where


import Avers
import Control.Monad.State
import Control.Applicative
import Storage.Objects.Boulder.Types


mkObjId :: Int -> Avers ObjId
mkObjId len = ObjId <$> liftIO (newId len)

mkStdObjId :: Avers ObjId
mkStdObjId = mkObjId 13

boulderObjectType :: ObjectType Boulder
boulderObjectType = ObjectType
    { otType   = "boulder"
    , otId     = mkStdObjId
    , otViews  = views
    }


------------------------------------------------------------------------------
-- Views

views :: [SomeView Boulder]
views =
    [ SomeView bouldersView
    ]


bouldersView :: View Boulder Boulder
bouldersView = View
    { viewName              = "boulders"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
