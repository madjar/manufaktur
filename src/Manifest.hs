{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Manifest (readManifest) where

import Import

import qualified RIO.Text as Text
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
    manifestDependencies <-
          HM.toList <$> traverse versionConstraintParser mods
    return Manifest {..}

versionConstraintParser :: Text -> Parser VersionConstraint
versionConstraintParser "*" = return Any
versionConstraintParser (Text.stripPrefix ">= " -> Just v) = return (GreaterThan v)
versionConstraintParser v | not (" " `Text.isInfixOf` v) = return (Equals v)
versionConstraintParser v = fail ("Unable to parse " <> show v <> " as a constraint parser.")

readManifest :: (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> m Manifest
readManifest file = do
  manifest_content <- readFileUtf8 file
  manifest <- either reportError return . parseTomlDoc file $ manifest_content
  either reportError return (parseEither manifestParser $ toJSON manifest)
