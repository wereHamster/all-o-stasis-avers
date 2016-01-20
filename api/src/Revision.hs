{-# LANGUAGE TemplateHaskell #-}

module Revision (revision) where

import System.IO.Error
import Language.Haskell.TH

revision :: Q Exp
revision = do
    rev <- runIO $ flip catchIOError (const $ pure Nothing) $ do
        body <- readFile "GIT-REVISION-FILE"
        return $ Just body

    [| rev |]
