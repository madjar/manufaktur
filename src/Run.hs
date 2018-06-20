{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Util
import ModPortal
import Dependencies

import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as BL

import Codec.Archive.Zip
import Data.Aeson.Encode.Pretty

run :: RIO App ()
run = do
  modList <- asks (optionsModList . appOptions)
  let lockFile = modList -<.> "lock.json"
  lockFileExists <- doesFileExist lockFile
  dependencies <- if lockFileExists
    then do logInfo (displayShow lockFile <> " exists, reading mods versions from it")
            readJSON lockFile
    else do logInfo (displayShow lockFile <> " does not exist, creating it")

            modpackDef <-
              filter (\t -> Text.index t 0 /= '#')
              . filter (not . Text.null)
              . Text.split (== '\n')
              <$> readFileUtf8 modList

            logDebug ("Mod list: " <> displayShow modpackDef)

            mods <- getMods
            mod <- resolveDeps mods modpackDef
            let dependencies = toList (foldMap flattenDeps mod)

            logInfo ("All dependencies: " <> displayShow (map name dependencies))

            writeFileBinary lockFile (BL.toStrict $ encodePretty dependencies)
            return dependencies

  writeModPack (modList -<.> "zip") dependencies

writeModPack :: FilePath -> [Mod a] -> RIO App ()
writeModPack output mods = do
  files <- traverse fetchMod mods
  logInfo ("Writing " <> displayShow output )
  createArchive output $ forM_ files $ \f -> do
    s <- mkEntrySelector ("modpack" </> takeFileName f)
    content <- readFileBinary f
    addEntry Store content s
