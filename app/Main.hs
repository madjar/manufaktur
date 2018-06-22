{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Directory
import RIO.Process
import qualified RIO.Text as Text
import Options.Applicative.Simple
import Lens.Micro.GHC
import qualified Paths_manufaktur

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_manufaktur.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> strArgument ( metavar "MODLIST"
                      <> help "A text file containing a list of mods"
                       )
    )
    empty
  let cacheDir = "cache"
  createDirectoryIfMissing False cacheDir
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  let envVar var = maybe (fail (Text.unpack var <> " environment variable required")) return
                   $ pc ^? envVarsL . ix var . to encodeUtf8
  username <- envVar "FACTORIO_USERNAME"
  token <- envVar "FACTORIO_TOKEN"
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appCacheDir = cacheDir
          , appFactorioUsername = username
          , appFactorioToken = token
          }
     in runRIO app run
