{-# LANGUAGE OverloadedStrings #-}

module Storage.ObjectTypes
    ( accountObjectType
    , boulderObjectType
    ) where


import Control.Monad.State
import Control.Applicative
import Avers

import Storage.Objects.Account.Types
import Storage.Objects.Boulder


mkObjId :: Int -> Avers ObjId
mkObjId len = ObjId <$> liftIO (newId len)

mkStdObjId :: Avers ObjId
mkStdObjId = mkObjId 13

accountObjectType :: ObjectType Account
accountObjectType = ObjectType
    { otType   = "account"
    , otId     = mkStdObjId
    , otViews  = []
    }
