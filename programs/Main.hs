module Main where

import Prelude ()
import Prelude.MH
import qualified Data.Text as T

import System.Exit ( exitFailure )

import Config
import Options
import App
import Events ( ensureKeybindingConsistency )


main :: IO ()
main = do
    opts <- grabOptions
    configResult <- findConfig (optConfLocation opts)
    config <- case configResult of
        Left err -> do
            putStrLn $ "Error loading config: " <> err
            exitFailure
        Right c -> return c

    case ensureKeybindingConsistency (configUserKeys config) of
        Right () -> return ()
        Left err -> do
            putStrLn $ "Configuration error: " <> err
            exitFailure

    let newConfig = config { configStateLocation = T.pack <$> optStateLocation opts
                           }

    finalSt <- runMatterhorn opts newConfig
    closeMatterhorn finalSt
