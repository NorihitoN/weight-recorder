{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Model.WeightRecord where

import Control.Exception (catch)
import Data.Functor.ProductIsomorphic ((|$|), (|*|))
import qualified Data.Time.LocalTime as TM
import Database.HDBC (IConnection, SqlError, SqlValue, withTransaction)
import qualified Database.HDBC.Record as DHR
import Database.Record.ToSql (ToSql)
import qualified Database.Relational as HRR
import Database.Relational.TH (makeRelationalRecordDefault)
import Entity.WeightRecord as WRecord
import GHC.Generics (Generic)
import System.IO (hPrint, stderr)

data NewRecord = NewRecord
  { nwrUserId :: !Int,
    nwrTime :: !TM.LocalTime,
    nwrWeight :: !Double
  }
  deriving (Show, Generic)

makeRelationalRecordDefault ''NewRecord

instance ToSql SqlValue NewRecord

piNewRecord :: HRR.Pi WRecord.WeightRecord NewRecord
piNewRecord = NewRecord |$| WRecord.userId' |*| WRecord.time' |*| WRecord.weight'

createRecord :: (IConnection c) => NewRecord -> c -> IO Integer
createRecord r conn = do
  let ins = HRR.typedInsert WRecord.tableOfWeightRecord piNewRecord
  withTransaction conn $
    \conn' ->
      DHR.runInsert conn' ins r
        `catch` \e -> do
          hPrint stderr (e :: SqlError)
          return 0

selectWRecord :: (IConnection c) => Int -> c -> IO [WRecord.WeightRecord]
selectWRecord uid conn = DHR.runQuery conn q uid
  where
    q :: HRR.Query Int WRecord.WeightRecord
    q =
      HRR.relationalQuery . HRR.relation' . HRR.placeholder $
        \ph -> do
          a <- HRR.query WRecord.weightRecord
          HRR.wheres $ a HRR.! WRecord.userId' HRR..=. ph
          HRR.desc $ a HRR.! WRecord.time'
          return a
