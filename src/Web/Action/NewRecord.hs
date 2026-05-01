{-# LANGUAGE OverloadedStrings #-}

module Web.Action.NewRecord
  ( getRecords
  , getNewRecord
  , postRecords
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import Data.Time.Clock (getCurrentTime)
import Database.HDBC.Sqlite3 (Connection)
import Model.WeightRecord (NewRecord (..), createRecord)
import Web.Core (requireLogin, redirectTo)
import Web.Scotty

getRecords :: ActionM ()
getRecords = do
  _ <- requireLogin
  html "<h1>Records</h1><a href='/records/new'>New Record</a>"

getNewRecord :: ActionM ()
getNewRecord = do
  _ <- requireLogin
  html
    "<form method='post' action='/records'>\
    \  <input type='number' step='0.1' name='weight' placeholder='Weight (kg)'><br>\
    \  <button type='submit'>Save</button>\
    \</form>"

postRecords :: Connection -> ActionM ()
postRecords conn = do
  userId <- requireLogin
  weight <- formParam "weight"
  now    <- liftIO $ getCurrentTime >>= \t ->
              utcToLocalTime <$> getCurrentTimeZone <*> pure t
  _      <- liftIO $ createRecord (NewRecord userId now weight) conn
  redirectTo "/records"
