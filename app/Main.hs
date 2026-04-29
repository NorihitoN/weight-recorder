{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HDBC.Sqlite3 (connectSqlite3)
import Web.Action.Register (getRegister, postRegister)
import Web.Scotty

main :: IO ()
main = do
  conn <- connectSqlite3 "weight.db"
  scotty 8080 $ do
    get "/register" getRegister
    post "/register" (postRegister conn)
