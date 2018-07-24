{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import ModPortal
import Dependencies
import Manifest

import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map

import Codec.Archive.Zip
import Data.Aeson.Encode.Pretty

run :: RIO App ()
run = do
  manifest <- readManifest "Manufaktur.toml"
  let lockFile = "Manufaktur.lock"
      outputFile = (manifest ^. name . to Text.unpack) <> "_" <> (manifest ^. version . to Text.unpack) <.> "zip"
  lockFileExists <- doesFileExist lockFile
  modpackContent <- if lockFileExists
    then do logInfo (displayShow lockFile <> " exists, reading mods versions from it")
            readJSON lockFile
    else do logInfo (displayShow lockFile <> " does not exist, creating it")

            let modList = manifest ^. dependencies

            logDebug ("Mod list: " <> displayShow modList)

            mods <- getMods
            modpackContent <- resolveDeps mods modList

            logInfo ("All modpackContent: " <> displayShow (Map.keys modpackContent))

            writeFileBinary lockFile (BL.toStrict $ encodePretty modpackContent)
            return modpackContent

  writeModPack outputFile (Map.elems modpackContent)

writeModPack :: FilePath -> [Release] -> RIO App ()
writeModPack output releases = do
  files <- traverse fetchRelease releases
  logInfo ("Writing " <> displayShow output )
  createArchive output $ forM_ files $ \f -> do
    s <- mkEntrySelector ("modpack" </> takeFileName f)
    content <- readFileBinary f
    addEntry Store content s
