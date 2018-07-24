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

import qualified RIO.Set as Set
import qualified RIO.Map as Map

import Lens.Micro.GHC
import Data.Aeson.Yak

resolveDeps :: Map Text Mod -> [Text] -> RIO App (Map Text Release)
resolveDeps modDB  mods = resolveDeps' modDB mods mempty

resolveDeps' :: Map Text Mod -> [Text] -> Map Text Release -> RIO App (Map Text Release)
resolveDeps' _ [] resolved = return resolved
resolveDeps' modDB (currentModName:rest) resolved =
  case Map.lookup currentModName modDB of
    Nothing -> do
      logWarn ("Unknown mod " <> displayShow currentModName <> ", skipping.")
      resolveDeps' modDB rest resolved
    Just currentMod -> do
      logDebug ("Resolving dependencies for " <> displayShow currentModName)
      modInfo <- getModInfo (currentMod ^. latestRelease)
      let dependenciesNames =
            modInfo ^.. dependencies . to shave . traverse .
            filtered (not . view option) .
            name .
            filtered (/= "base")
          unresolvedDependencies =
            Set.fromList dependenciesNames `Set.difference` Map.keysSet resolved
                    --TODO dedup
      resolveDeps'
        modDB
        (rest ++ Set.toList unresolvedDependencies)
        (Map.insert (currentMod ^. name) (currentMod ^. latestRelease) resolved)
