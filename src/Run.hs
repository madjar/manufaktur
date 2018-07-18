{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Util
import ModPortal
import Dependencies
import Manifest

import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as BL

import Codec.Archive.Zip
import Data.Aeson.Encode.Pretty
import Lens.Micro.GHC

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

            let modList = manifest ^.. dependencies . traverse . _1

            logDebug ("Mod list: " <> displayShow modList)

            mods <- getMods
            mod_ <- resolveDeps mods modList
            let modpackContent= toList (foldMap flattenDeps mod_)

            logInfo ("All modpackContent: " <> displayShow (map modName modpackContent))

            writeFileBinary lockFile (BL.toStrict $ encodePretty modpackContent)
            return modpackContent

  writeModPack outputFile modpackContent

writeModPack :: FilePath -> [Mod a] -> RIO App ()
writeModPack output mods = do
  files <- traverse fetchMod mods
  logInfo ("Writing " <> displayShow output )
  createArchive output $ forM_ files $ \f -> do
    s <- mkEntrySelector ("modpack" </> takeFileName f)
    content <- readFileBinary f
    addEntry Store content s
