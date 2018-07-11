{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Dependencies
  ( resolveDeps
  , flattenDeps
  , ModInfo (..)
  ) where

import Import
import ModPortal

import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map
import qualified RIO.List as List
import qualified RIO.Text as Text
import RIO.FilePath

import Data.Aeson
import Data.Fix
import Codec.Archive.Zip
import Lens.Micro.TH
import Data.Aeson.TH
import Data.Aeson.Yak

data Dependency = Dependency
  { dependencyName :: Text
  , dependencyOption :: Bool
  , dependencyContraint :: Text
  } deriving (Show)
makeFields ''Dependency

instance FromJSON Dependency where
  parseJSON = withText "dependency" $ \s -> do
    let (opt, s') = case Text.stripPrefix "? " s of
          Just x -> (True, x)
          Nothing -> (False, s)
        (n, c) = Text.break (== ' ') s'
    -- TODO actual parsing of constraints (use an actual parsing library? Or at least regexes?)
    return (Dependency n opt c)


data ModInfo = ModInfo {modInfoName :: Text, modInfoVersion :: Text, modInfoFactorioVersion :: Text, modInfoDependencies :: Yak Dependency} deriving (Show)
makeFields ''ModInfo
deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop (Text.length "modInfo")} '' ModInfo

getModInfo :: Mod a -> RIO App ModInfo
getModInfo mod_ = do
  modFile <- fetchMod mod_
  infoEntry <- withArchive modFile $ do
    s <- mkEntrySelector (dropExtension (modFilename mod_) </> "info.json")
    getEntry s

  either fail return (eitherDecodeStrict infoEntry)

lookupDependencies :: Mod a -> RIO App (Mod Text)
lookupDependencies mod_ = do
  modInfo <- getModInfo mod_
  let toName = map (view name)
      sortedDeps =
        (toName *** toName) .
        List.partition (view option) .
        filter ((/= "base") . view name) . shave . view dependencies $
        modInfo
  logDebug
    ("Dependencies for " <> displayShow (modName mod_) <> " are " <>
     displayShow sortedDeps)
  return (mod_ {modDependencies = Just sortedDeps})

resolveDeps :: Map Text (Mod Text) -> [Text] -> RIO App [Fix Mod]
resolveDeps mods names = do
  go <- caching (\n -> lookupDependencies (mods Map.! n))
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
