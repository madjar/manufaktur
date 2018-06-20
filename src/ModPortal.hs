{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ModPortal
  ( getMods
  , fetchMod
  ) where

import Import
import Util

import RIO.Directory
import RIO.FilePath
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map
import qualified RIO.Map.Unchecked as Map


import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Network.HTTP.Simple


getMods :: RIO App (Map Text (Mod Text))
getMods = do
  cacheDir <- asks appCacheDir
  let modsFile = cacheDir </> "mods.json"
  emods <- tryAny $ readJSON modsFile
  mods <- case emods of
    Right mods -> return mods
    Left _ -> do
      logInfo "Downloading mods list"
      response <- httpJSON "https://mods.factorio.com/api/mods?page_size=10000"
      let mods = either error id $ parseEither modsResponseParser (getResponseBody response)
      writeFileBinary modsFile (BL.toStrict (encode mods))
      return mods
  return . Map.fromDistinctAscList . map (name &&& id ) $ mods

modsResponseParser :: Object -> Parser [Mod Text]
modsResponseParser root = do
  results <- root .: "results"
  forM results $ \obj -> do
    name <- obj .: "name"
    latest_release <- obj .: "latest_release"
    file_name <- latest_release .: "file_name"
    download_url <- latest_release .: "download_url"
    return Mod { name = name, filename = file_name, downloadUrl = download_url, dependencies = Nothing}


fetchMod :: Mod a -> RIO App FilePath
fetchMod mod = do
  cacheDir <- asks appCacheDir
  let modFile = cacheDir </> filename mod
  whenM (not <$> doesFileExist modFile) $ do
    logInfo ("Downloading " <> displayShow (filename mod))
    -- XXX file in memory
    response <- httpBS (setRequestPath (encodeUtf8 (downloadUrl mod)) "https://mods.factorio.com?username=madjar&token=dd2965ef6a27cbe0fd6e0a31adea7e")
    writeFileBinary modFile (getResponseBody response)
  return modFile
