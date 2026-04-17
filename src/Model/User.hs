{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Model.User where

import Control.Exception (catch)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import qualified Data.ByteString as BS
import Data.Functor.ProductIsomorphic ((|$|), (|*|))
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.HDBC (IConnection, SqlError, SqlValue, withTransaction)
import qualified Database.HDBC.Record as DHR
import Database.Record.ToSql (ToSql)
import qualified Database.Relational as HRR
import Database.Relational.TH (makeRelationalRecordDefault)
import qualified Entity.User as User
import GHC.Generics (Generic)
import System.IO (hPrint, hPutStrLn, stderr)

data NewUser = NewUser
  { nuName :: String,
    nuPassword :: String
  }
  deriving (Show, Generic)

makeRelationalRecordDefault ''NewUser

instance ToSql SqlValue NewUser

piNewUser :: HRR.Pi User.User NewUser
piNewUser = NewUser |$| User.name' |*| User.password'

insertUser :: (IConnection c) => NewUser -> c -> IO Integer
insertUser u conn = do
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy $ enc . nuPassword $ u
  case mHash of
    Nothing -> do
      hPutStrLn stderr "Failed to hash password"
      return 0
    Just hash -> do
      let ins = HRR.typedInsert User.tableOfUser piNewUser
          u' = u {nuPassword = dec hash}
      withTransaction conn $
        \conn' -> do
          DHR.runInsert conn' ins u'
            `catch` \e -> do
              hPrint stderr (e :: SqlError)
              return 0

enc :: String -> BS.ByteString
enc = encodeUtf8 . pack

dec :: BS.ByteString -> String
dec = unpack . decodeUtf8
