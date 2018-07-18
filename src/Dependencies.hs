{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Dependencies
  ( resolveDeps
  , flattenDeps
  , ModInfo (..)
  ) where

import Import
import Util
import ModPortal

import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified RIO.List as List

import Data.Fix
import Data.Aeson.Yak

lookupDependencies :: Mod a -> RIO App (Mod Text)
lookupDependencies mod_ = do
  modInfo <- getModInfo mod_
  let toName = map (view name)
      sortedDeps =
        (toName *** toName) .
        List.partition (not . view option) .
        filter ((/= "base") . view name) . shave . view dependencies $
        modInfo
  logDebug
    ("Dependencies for " <> displayShow (modName mod_) <> " are " <>
     displayShow sortedDeps)
  return (mod_ {modDependencies = Just sortedDeps})

resolveDeps :: Map Text (Mod Text) -> [Text] -> RIO App [Fix Mod]
resolveDeps mods names = do
  go <- caching $ \depName ->
    case Map.lookup depName mods of
      Just dep -> lookupDependencies dep
      Nothing -> reportError ("Could not find mod " <> depName)

  traverse (anaM go) names

flattenDeps :: Fix Mod -> Set (Mod ())
flattenDeps = Set.fromList . cata go
  where go mod_ = let Just (mandatory, _) = modDependencies mod_ in mod_ {modDependencies = Nothing} : concat mandatory

caching :: (Ord k, MonadIO m) => (k -> m a) -> m (k -> m a)
caching f = do
  cacheRef <- newIORef Map.empty
  return $ \n -> do
    cache <- readIORef cacheRef
    case Map.lookup n cache of
      Just r -> return r
      Nothing -> do
        r <- f n
        modifyIORef' cacheRef (Map.insert n r)
        return r
