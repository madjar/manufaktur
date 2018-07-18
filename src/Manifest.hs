{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Manifest (readManifest) where

import Import

import qualified RIO.HashMap as HM

import Text.Toml
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)


manifestParser :: Value -> Parser Manifest
manifestParser =
  withObject "manifest" $ \root -> do
    modpack <- root .: "modpack"
    manifestName <- modpack .: "name"
    manifestVersion <- modpack .: "version"
    mods <- root .: "mods"
    let manifestDependencies =
          HM.toList . HM.map (\c -> guard (c /= "*") >> Just c) $ mods
    return Manifest {..}

readManifest :: (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> m Manifest
readManifest file = do
  manifest_content <- readFileUtf8 file
  manifest <- either reportError return . parseTomlDoc file $ manifest_content
  either reportError return (parseEither manifestParser $ toJSON manifest)
