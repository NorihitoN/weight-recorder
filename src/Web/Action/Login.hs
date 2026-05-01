{-# LANGUAGE OverloadedStrings #-}

module Web.Action.Login
  ( getLogin,
    postLogin,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Database.HDBC.Sqlite3 (Connection)
import qualified Entity.User as EU
import Model.User (selectUser)
import Web.Core (redirectTo, renderTemplate, setSessionUserId)
import Web.Scotty

getLogin :: ActionM ()
getLogin = renderTemplate "login.mustache" (object [])

postLogin :: Connection -> ActionM ()
postLogin conn = do
  name <- formParam "name"
  pass <- formParam "password"
  mUser <- liftIO $ selectUser name pass conn
  case mUser of
    Nothing ->
      renderTemplate "login.mustache"
        (object ["error" .= ("ユーザー名またはパスワードが違います" :: String)])
    Just user -> do
      setSessionUserId (EU.id user)
      redirectTo "/records"
