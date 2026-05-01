{-# LANGUAGE OverloadedStrings #-}

module Web.Action.Login
  ( getLogin,
    postLogin,
  )
where

import Control.Monad.IO.Class (liftIO)
import Database.HDBC.Sqlite3 (Connection)
import qualified Entity.User as EU
import Model.User (selectUser)
import Web.Core (redirectTo, setSessionUserId)
import Web.Scotty

getLogin :: ActionM ()
getLogin =
  html
    "<form method='post' action='/login'>\
    \  <input type='text'     name='name'     placeholder='Username'><br>\
    \  <input type='password' name='password' placeholder='Password'><br>\
    \  <button type='submit'>Login</button>\
    \</form>"

postLogin :: Connection -> ActionM ()
postLogin conn = do
  name <- formParam "name"
  pass <- formParam "password"
  mUser <- liftIO $ selectUser name pass conn
  case mUser of
    Nothing -> html "Invalid name or password"
    Just user -> do
      setSessionUserId (EU.id user)
      redirectTo "/records"
