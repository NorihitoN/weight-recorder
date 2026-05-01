{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Web.Router as Router
import Web.Scotty

main :: IO ()
main = do
  conn <- connectSqlite3 "weight.db"
  scotty 8080 $ Router.routes conn
