{-# LANGUAGE OverloadedStrings #-}

module Config (config, module Config.Types) where

import Config.Types

config :: Config
config = Config
    { cPort = 8000

      -- TODO: Find a way to parse the URL at compile time.
    , cRethinkDB = case parseRelativeReference "//localhost/allostasis" of
        Nothing -> error "config"
        Just x -> x

    , cPassport = PassportConfig
        { pcRealm = "Minimum Boulder App"
        , pcFrom = "auth@boulderapp.com"
        , pcApiDomain = "http://localhost:8000"
        , pcAppDomain = "http://localhost:8081"
        , pcSendProvider = PCSPTerminal
        }
    }
