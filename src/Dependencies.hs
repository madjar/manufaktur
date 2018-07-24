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

resolveDeps :: Map Text Mod -> Map Text Release -> [(Text, VersionConstraint)] -> RIO App (Map Text Release)
resolveDeps modDB lockFile mods = resolveDeps' modDB lockFile mods mempty

resolveDeps' :: Map Text Mod -> Map Text Release -> [(Text, VersionConstraint)] -> Map Text Release -> RIO App (Map Text Release)
resolveDeps' _ _ [] resolved = return resolved
resolveDeps' modDB lockFile ((currentModName, currentConstraint):rest) resolved =
  case Map.lookup currentModName modDB of
    Nothing -> do
      logWarn ("Unknown mod " <> displayShow currentModName <> ", skipping.")
      resolveDeps' modDB lockFile rest resolved
    Just currentMod -> do
      let chosenRelease =
            case Map.lookup currentModName lockFile of
              Nothing -> currentMod ^. latestRelease
              Just r -> r
          chosenVersion = chosenRelease ^. version
      case currentConstraint of
        Any -> return ()
        Equals v ->
          when
            (v /= chosenVersion)
            (reportError
               ("Problem with the requirement for mod " <> show currentModName <>
                ": version " <>
                show v <>
                " is required, but the algo is silly and will only use the last version (" <>
                show chosenVersion <>
                ")"))
        GreaterThan v ->
          when
            (not $ chosenVersion `versionIsGreaterThan` v)
            (reportError
               ("Problem with the requirement for mod " <> show currentModName <>
                ": version " <>
                show v <>
                " or greater is required, but the last version is " <>
                show chosenVersion))
      logDebug
        ("Resolving " <> displayShow currentModName <> " to " <>
         displayShow chosenVersion)
      modInfo <- getModInfo chosenRelease
      let dependenciesNames =
            modInfo ^.. dependencies . to shave . traverse .
            filtered (not . view option) .
            filtered ((/= "base") . view name)
          unresolvedDependencies =
            filter
              (\d -> view name d `Map.notMember` resolved)
              dependenciesNames
          dependenciesToConsider =
            filter
              (\d -> view name d `notElem` map fst rest) -- XXX this is ugly and slow
              unresolvedDependencies
      resolveDeps'
        modDB
        lockFile
        (rest ++ map depAsTuple dependenciesToConsider)
        (Map.insert (currentMod ^. name) (currentMod ^. latestRelease) resolved)

depAsTuple :: (HasName s a, HasConstraint s b) => s -> (a, b)
depAsTuple d = (view name d, view constraint d)
