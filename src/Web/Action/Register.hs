{-# LANGUAGE OverloadedStrings #-}

module Web.Action.Register
  ( getRegister,
    postRegister,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Database.HDBC.Sqlite3 (Connection)
import Model.User (NewUser (..), createUser)
import Web.Core (redirectTo, renderTemplate)
import Web.Scotty

getRegister :: ActionM ()
getRegister = renderTemplate "register.mustache" (object [])

postRegister :: Connection -> ActionM ()
postRegister conn = do
  name <- formParam "name"
  pass <- formParam "password"
  result <- liftIO $ try $ createUser (NewUser name pass) conn
  case (result :: Either SomeException Integer) of
    Left _  -> renderTemplate "register.mustache"
                 (object ["error" .= ("そのユーザー名は既に使われています" :: String)])
    Right _ -> redirectTo "/login"
