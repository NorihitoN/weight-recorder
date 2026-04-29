{-# LANGUAGE OverloadedStrings #-}

module Web.Action.Register
  ( getRegister,
    postRegister,
  )
where

import Control.Monad.IO.Class (liftIO)
import Database.HDBC.Sqlite3 (Connection)
import Model.User (NewUser (..), createUser)
import Web.Core (redirectTo)
import Web.Scotty

getRegister :: ActionM ()
getRegister =
  html
    "<form method='post' action='/register'>\
    \  <input type='text'     name='name'     placeholder='Username'><br>\
    \  <input type='password' name='password' placeholder='Password'><br>\
    \  <button type='submit'>Register</button>\
    \</form>"

postRegister :: Connection -> ActionM ()
postRegister conn = do
  name <- formParam "name"
  pass <- formParam "password"
  _ <- liftIO $ createUser (NewUser name pass) conn
  redirectTo "/login"
