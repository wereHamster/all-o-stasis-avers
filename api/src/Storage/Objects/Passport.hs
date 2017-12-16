{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Passport
    ( module Storage.Objects.Passport.Types

    , passportObjectType

    , passportsView
    , unconfirmedPassportsView
    , validPassportsView
    ) where


import Avers
import Control.Monad.State
import Storage.Objects.Passport.Types

mkObjId :: Int -> Avers ObjId
mkObjId len = ObjId <$> liftIO (newId len)

passportViews :: [SomeView Passport]
passportViews =
    [ SomeView passportsView
    , SomeView unconfirmedPassportsView
    , SomeView validPassportsView
    ]

passportObjectType :: ObjectType Passport
passportObjectType = ObjectType
    { otType   = "passport"
    , otId     = mkObjId 42
    , otViews  = passportViews
    }

passportsView :: View Passport Passport
passportsView = View
    { viewName              = "passports"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }

unconfirmedPassportsView :: View Passport Passport
unconfirmedPassportsView = View
    { viewName              = "unconfirmedPassports"
    , viewParser            = parseDatum
    , viewObjectTransformer = \passport -> case passportValidity passport of
        PVUnconfirmed -> pure (Just passport)
        PVValid       -> pure Nothing
        PVExpired     -> pure Nothing
    , viewIndices           = []
    }

validPassportsView :: View Passport Passport
validPassportsView = View
    { viewName              = "validPassports"
    , viewParser            = parseDatum
    , viewObjectTransformer = \passport -> case passportValidity passport of
        PVUnconfirmed -> pure Nothing
        PVValid       -> pure (Just passport)
        PVExpired     -> pure Nothing
    , viewIndices           = []
    }
