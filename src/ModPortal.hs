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
  , fetchMod
  , Dependency(..), name, option, constraint
  , ModInfo (..), version, dependencies
  , getModInfo
  ) where

import Import
import Util

import RIO.Directory
import RIO.FilePath
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map.Unchecked as Map
import qualified RIO.Text as Text

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Network.HTTP.Simple
import Network.HTTP.Conduit (parseUrlThrow)
import Lens.Micro.TH
import Data.Aeson.TH
import Data.Aeson.Yak
import qualified Text.Regex.Applicative.Text as Regex
import Codec.Archive.Zip

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
  return . Map.fromDistinctAscList . map (modName &&& id ) $ mods

modsResponseParser :: Object -> Parser [Mod Text]
modsResponseParser root = do
  results <- root .: "results"
  forM results $ \obj -> do
    name <- obj .: "name"
    latest_release <- obj .: "latest_release"
    file_name <- latest_release .: "file_name"
    download_url <- latest_release .: "download_url"
    return Mod { modName = name, modFilename = file_name, modDownloadUrl = download_url, modDependencies = Nothing}


fetchMod :: Mod a -> RIO App FilePath
fetchMod Mod {modFilename, modDownloadUrl} = do
  cacheDir <- asks appCacheDir
  token <- asks appFactorioToken
  username <- asks appFactorioUsername
  let modFile = cacheDir </> modFilename
  whenM (not <$> doesFileExist modFile) $ do
    logInfo ("Downloading " <> displayShow (modFilename))
    req <- parseUrlThrow "https://mods.factorio.com"
    -- XXX file in memory
    response <- httpBS
      $ setRequestPath (encodeUtf8 modDownloadUrl)
      $ setRequestQueryString [("username", Just username), ("token", Just token)]
      $ req
    writeFileBinary modFile (getResponseBody response)
  return modFile


data Dependency = Dependency
  { dependencyOption :: Bool
  , dependencyName :: Text
  , dependencyConstraint :: Maybe Text
  } deriving (Show)
makeFields ''Dependency

instance FromJSON Dependency where
  parseJSON = withText "dependency" $ \s -> maybe (fail ("Could not parse " <> show s)) pure (Regex.match regex s)
      where regex = Dependency <$> optionR <*> nameR <*> constraintR
            optionR = isJust <$> optional (Regex.string "? ")
            nameR = Text.pack <$> some (Regex.psym (/= ' '))
            constraintR = optional $ Text.pack <$ Regex.string " >= " <*> some Regex.anySym


data ModInfo = ModInfo
  { modInfoName :: Text
  , modInfoVersion :: Text
  , modInfoFactorioVersion :: Text
  , modInfoDependencies :: Yak Dependency
  } deriving (Show)
makeFields ''ModInfo
deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop (Text.length "modInfo")} '' ModInfo

getModInfo :: Mod a -> RIO App ModInfo
getModInfo mod_ = do
  modFile <- fetchMod mod_
  infoEntry <- withArchive modFile $ do
    s <- mkEntrySelector (dropExtension (modFilename mod_) </> "info.json")
    getEntry s

  either fail return (eitherDecodeStrict infoEntry)
