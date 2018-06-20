{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Dependencies
  ( resolveDeps
  , flattenDeps
  ) where

import Import
import ModPortal

import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map
import qualified RIO.Text as Text
import RIO.FilePath

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Fix
import Codec.Archive.Zip

-- Forgot why
blackList = ["UberTweaks"]


lookupDependencies :: Mod a -> RIO App (Mod Text)
lookupDependencies mod = do
  modFile <- fetchMod mod
  infoEntry <- withArchive modFile $ do
    s <- mkEntrySelector (dropExtension (filename mod) </> "info.json")
    getEntry s

  let allDeps = either error id $ parseEither (\o -> o .:? "dependencies" .!= []) =<< eitherDecodeStrict infoEntry
      allSortedDeps = partitionEithers (map (\s -> maybe (Left s) Right (Text.stripPrefix "? " s)) allDeps)
      cleanDeps = filter (/= "base") . filter (`notElem` blackList) . map (Text.strip . Text.takeWhile (`notElem` ['<', '>', '=']))
      sortedDeps = (cleanDeps *** cleanDeps) allSortedDeps

  logDebug ("Dependencies for " <> displayShow (name mod) <> " are " <> displayShow sortedDeps)

  return (mod { dependencies = Just sortedDeps })

resolveDeps :: Map Text (Mod Text) -> [Text] -> RIO App [Fix Mod]
resolveDeps mods names = do
  go <- caching (\n -> lookupDependencies (mods Map.! n))
  traverse (anaM go) names

flattenDeps :: Fix Mod -> Set (Mod ())
flattenDeps = Set.fromList . cata go
  where go mod = let Just (mandatory, _) = dependencies mod in mod {dependencies = Nothing} : concat mandatory

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
