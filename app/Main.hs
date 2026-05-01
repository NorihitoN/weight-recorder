{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HDBC.Sqlite3 (connectSqlite3)
import Web.Action.Login (getLogin, postLogin)
import Web.Action.NewRecord (getNewRecord, getRecords, postRecords)
import Web.Action.Register (getRegister, postRegister)
import Web.Scotty

main :: IO ()
main = do
  conn <- connectSqlite3 "weight.db"
  scotty 8080 $ do
    get  "/register"   getRegister
    post "/register"   (postRegister conn)
    get  "/login"      getLogin
    post "/login"      (postLogin conn)
    get  "/records"    getRecords
    get  "/records/new" getNewRecord
    post "/records"    (postRecords conn)
