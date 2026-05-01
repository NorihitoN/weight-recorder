{-# LANGUAGE OverloadedStrings #-}

module Web.Action.NewRecord
  ( getRecords,
    postRecords,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Database.HDBC.Sqlite3 (Connection)
import qualified Entity.User as EU
import qualified Entity.WeightRecord as EW
import Model.User (selectUserById)
import Model.WeightRecord (NewRecord (..), createRecord, selectWRecord)
import Web.Core (redirectTo, renderTemplate, requireLogin)
import Web.Scotty

getRecords :: Connection -> ActionM ()
getRecords conn = do
  uid   <- requireLogin
  recs  <- liftIO $ selectWRecord uid conn
  mUser <- liftIO $ selectUserById uid conn
  let uname   = maybe "" EU.name mUser
      recList = map toRecordObj recs
      hasRecs = not (null recs)
  renderTemplate "records.mustache" $
    object [ "username"   .= uname
           , "records"    .= recList
           , "hasRecords" .= hasRecs
           ]
  where
    toRecordObj r = object
      [ "time"   .= show (EW.time r)
      , "weight" .= EW.weight r
      ]

postRecords :: Connection -> ActionM ()
postRecords conn = do
  userId <- requireLogin
  weight <- formParam "weight"
  now    <- liftIO $ getCurrentTime >>= \t ->
              utcToLocalTime <$> getCurrentTimeZone <*> pure t
  _      <- liftIO $ createRecord (NewRecord userId now weight) conn
  redirectTo "/records"
