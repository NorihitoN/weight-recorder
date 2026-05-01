{-# LANGUAGE OverloadedStrings #-}

module Web.Router (routes) where

import Database.HDBC.Sqlite3 (Connection)
import Web.Action.Login (getLogin, postLogin)
import Web.Action.NewRecord (getRecords, postRecords)
import Web.Action.Register (getRegister, postRegister)
import Web.Scotty

routes :: Connection -> ScottyM ()
routes conn = do
  get  "/register" getRegister
  post "/register" (postRegister conn)
  get  "/login"    getLogin
  post "/login"    (postLogin conn)
  get  "/records"  (getRecords conn)
  post "/records"  (postRecords conn)
