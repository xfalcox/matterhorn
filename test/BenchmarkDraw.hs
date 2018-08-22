module Main where

import Brick (render)
import Criterion.Main
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Draw
import Types

usage :: IO a
usage = do
    n <- getProgName
    putStrLn $ "Usage: " <> n <> " <state file>"
    exitFailure

main :: IO ()
main = do
    args <- getArgs

    stateFilePath <- case args of
        [p] -> return p
        _ -> usage

    stateBytes <- BSL.readFile stateFilePath
    state <- case A.eitherDecode stateBytes :: Either String ChatState of
        Left e -> do
            putStrLn $ "Error decoding state file: " <> e
            exitFailure
        Right s -> return s

    -- defaultMain [bench "draw" $ nf (fmap render . draw) state]
    putStrLn "Done!"
