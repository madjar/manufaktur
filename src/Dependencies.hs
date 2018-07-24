{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Dependencies
  ( resolveDeps
  , ModInfo (..)
  ) where

import Import
import ModPortal

import qualified RIO.Map as Map
import qualified RIO.Text as Text

import Lens.Micro.GHC
import Data.Aeson.Yak

versionIsGreaterThan :: Text -> Text -> Bool
versionIsGreaterThan x y = parse x >= parse y
  where parse :: Text -> [Maybe Int]
        parse = map (readMaybe . Text.unpack) . Text.split (== '.')

resolveDeps :: Map Text Mod -> [(Text, VersionConstraint)] -> RIO App (Map Text Release)
resolveDeps modDB  mods = resolveDeps' modDB mods mempty

resolveDeps' :: Map Text Mod -> [(Text, VersionConstraint)] -> Map Text Release -> RIO App (Map Text Release)
resolveDeps' _ [] resolved = return resolved
resolveDeps' modDB ((currentModName, currentConstraint):rest) resolved =
  case Map.lookup currentModName modDB of
    Nothing -> do
      logWarn ("Unknown mod " <> displayShow currentModName <> ", skipping.")
      resolveDeps' modDB rest resolved
    Just currentMod -> do
      case currentConstraint of
        Any -> return ()
        Equals v ->
          when
            (v /= currentMod ^. latestRelease . version)
            (reportError
               ("Problem with the requirement for mod " <> show currentModName <>
                ": version " <>
                show v <>
                " is required, but the algo is silly and will only use the last version (" <>
                show (currentMod ^. latestRelease . version) <>
                ")"))
        GreaterThan v ->
          when
            (not $ (currentMod ^. latestRelease . version) `versionIsGreaterThan` v)
            (reportError
               ("Problem with the requirement for mod " <> show currentModName <>
                ": version " <>
                show v <>
                " or greater is required, but the last version is " <>
                show (currentMod ^. latestRelease . version)))
      logDebug ("Resolving dependencies for " <> displayShow currentModName)
      modInfo <- getModInfo (currentMod ^. latestRelease)
      let dependenciesNames =
            modInfo ^.. dependencies . to shave . traverse .
            filtered (not . view option) .
            filtered ((/= "base") . view name)
          unresolvedDependencies =
            filter
              (\d -> view name d `Map.notMember` resolved)
              dependenciesNames
                    --TODO dedup
      resolveDeps'
        modDB
        (rest ++ map depAsTuple unresolvedDependencies)
        (Map.insert (currentMod ^. name) (currentMod ^. latestRelease) resolved)

depAsTuple :: (HasName s a, HasConstraint s b) => s -> (a, b)
depAsTuple d = (view name d, view constraint d)
