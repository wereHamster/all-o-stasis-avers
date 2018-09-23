{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PassportConfirmationEmail
    ( passportConfirmationEmail
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Monoid

import           Network.Mail.Mime

import           PassportAuth


passportConfirmationEmail :: PassportConfig -> Text -> Text -> Text -> Text -> Mail
passportConfirmationEmail PassportConfig{..} email passportId securityCode confirmationToken = simpleMail'
    (Address Nothing email)
    (Address Nothing _pcFrom)
    subject
    body
  where
    -- TODO: Use servant to render the URL.
    confirmationUrl = _pcApiDomain <> "/login/confirm?passportId=" <> passportId <> "&confirmationToken=" <> confirmationToken

    subject = _pcRealm <> " Login Verification (code: \"" <> securityCode <> "\")"

    body =  LT.fromStrict $ T.intercalate "\n"
        [ "Verify your email to log on to the " <> _pcRealm
        , ""
        , "We have received a login attempt with the following code:"
        , ""
        , securityCode
        , ""
        , "To complete the login process, please click the URL below:"
        , ""
        , confirmationUrl
        , ""
        , "Or copy and paste this URL into your browser."
        ]
