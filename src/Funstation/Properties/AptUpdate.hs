{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Funstation.Properties.AptUpdate where

import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import Shh (devNull, (&>))
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Text qualified as T
import System.Environment (getEnv)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.IO.Class (liftIO)

data AptUpdateP = AptUpdateP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Re-run @apt-get update@ only if last run over 1 day ago
aptUpdateIntervalSeconds :: Integer
aptUpdateIntervalSeconds = 60 * 60 * 24

-- | path to last successful @apt-get update@ time.
aptUpdateTsFile :: FilePath -> FilePath
aptUpdateTsFile home = home <> "/.local/state/funstation/apt-update/last-run-ts"

-- | Read the last-run POSIX timestamp; returns 0 if absent.
getLastAptUpdateTs :: FilePath -> IO Integer
getLastAptUpdateTs home = do
  let tsFile = aptUpdateTsFile home
  exists <- doesFileExist tsFile
  if not exists
    then return 0
    else do
      content <- readFile tsFile
      return $ read $ T.unpack $ T.strip $ T.pack content

saveLastAptUpdateTs :: FilePath -> IO ()
saveLastAptUpdateTs home = do
  let dir = home <> "/.local/state/funstation/apt-update"
  createDirectoryIfMissing True dir
  now <- round <$> getPOSIXTime
  writeFile (aptUpdateTsFile home) (show (now :: Integer) <> "\n")

instance Prop AptUpdateP where
  desc _ = "apt package lists updated"
  attrs _ = mempty

  -- Fresh iff last successful update within interval.
  checker _ = do
    home    <- liftIO $ getEnv "HOME"
    now     <- liftIO $ round <$> getPOSIXTime
    lastRun <- liftIO $ getLastAptUpdateTs home
    return $ (lastRun + aptUpdateIntervalSeconds) >= now

  fixer _ = do
    result <- runCmd ["sudo", "apt-get", "update"] (&> devNull)
    case result of
      Right _ -> do
        home <- liftIO $ getEnv "HOME"
        liftIO $ saveLastAptUpdateTs home
        putStrLn' "apt package sets updated successfully"
      -- Don't record a timestamp on failure, next run should retry.
      Left err -> putStrLn' $ "Failed to update apt: " <> tshow err

  dependencies _ = return []
