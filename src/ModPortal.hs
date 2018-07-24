{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ModPortal
  ( getMods
  , fetchRelease
  , Dependency(..), name, option, constraint
  , ModInfo (..), version, dependencies
  , getModInfo
  ) where

import Import

import RIO.Directory
import RIO.FilePath
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map.Unchecked as Map

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Network.HTTP.Simple
import Network.HTTP.Conduit (parseUrlThrow)
import Codec.Archive.Zip

getMods :: RIO App (Map Text Mod)
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
  return . Map.fromDistinctAscList . map (modName &&& id ) $ mods

modsResponseParser :: Object -> Parser [Mod]
modsResponseParser root = do
  root .: "results"


fetchRelease :: Release -> RIO App FilePath
fetchRelease r = do
  cacheDir <- asks appCacheDir
  token <- asks appFactorioToken
  username <- asks appFactorioUsername
  let modFile = cacheDir </> view fileName r
  whenM (not <$> doesFileExist modFile) $ do
    logInfo ("Downloading " <> displayShow (view fileName r))
    req <- parseUrlThrow "https://mods.factorio.com"
    -- XXX file in memory
    response <- httpBS
      $ setRequestPath (r ^. downloadUrl . to encodeUtf8)
      $ setRequestQueryString [("username", Just username), ("token", Just token)]
      $ req
    writeFileBinary modFile (getResponseBody response)
  return modFile


getModInfo :: Release -> RIO App ModInfo
getModInfo r = do
  modFile <- fetchRelease r
  infoEntry <- withArchive modFile $ do
    s <- mkEntrySelector (dropExtension (view fileName r) </> "info.json")
    getEntry s

  either fail return (eitherDecodeStrict infoEntry)
