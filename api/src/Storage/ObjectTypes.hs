{-# LANGUAGE OverloadedStrings #-}

module Storage.ObjectTypes
    ( accountObjectType
    , boulderObjectType
    ) where


import Control.Monad.State
import Control.Applicative
import Avers

import Storage.Objects.Account
import Storage.Objects.Boulder
