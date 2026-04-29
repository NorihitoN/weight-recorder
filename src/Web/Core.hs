{-# LANGUAGE OverloadedStrings #-}

module Web.Core where

import qualified Data.Text.Lazy as TL
import Database.HDBC.Sqlite3 (Connection)
import Web.Scotty

type AppAction = Connection -> ActionM ()

getSessionUserId :: ActionM (Maybe Int)
setSessionUserId :: Int -> ActionM ()
clearSession :: ActionM ()
redirectTo :: String -> ActionM ()
requireLogin :: ActionM Int

setSessionUserId uid =
  setHeader
    "Set-Cookie"
    (TL.pack $ "session=" ++ show uid ++ "; HttpOnly; Path=/")

getSessionUserId = do
  cookieH <- header "Cookie"
  return $ cookieH >>= parseCookie

clearSession =
  setHeader
    "Set-Cookie"
    (TL.pack "session=; HttpOnly; Path=/; Max-Age=0")

redirectTo url = redirect (TL.pack url)

requireLogin = do
  mUid <- getSessionUserId
  case mUid of
    Nothing -> redirectTo "/login" >> return 0
    Just uid -> return uid

parseCookie :: TL.Text -> Maybe Int
parseCookie cookie =
  let parts = map TL.strip $ TL.splitOn ";" cookie
      findSession [] = Nothing
      findSession (p : ps) =
        let (k, v) = TL.breakOn "=" p
         in if TL.strip k == TL.pack "session"
              then case reads (TL.unpack (TL.drop 1 v)) of
                [(n, "")] -> Just n
                _ -> Nothing
              else findSession ps
   in findSession parts
