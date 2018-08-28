module Main where

import qualified Brick as B
import Criterion.Main
import Criterion.Types (Config(..), Verbosity(..))
import Control.Exception (finally)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Graphics.Vty hiding (defaultConfig)
import Lens.Micro.Platform
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Draw
import Types

usage :: IO a
usage = do
    n <- getProgName
    putStrLn $ "Usage: " <> n <> " <state file>"
    exitFailure

doBuild :: SerializedState -> (B.RenderState Name, Picture, Maybe (B.CursorLocation Name), [B.Extent Name])
doBuild ss =
    let cs = serializedChatState ss
        rs = B.resetRenderState $ serializedRenderState ss
    in B.renderFinal (cs^.csResources.crTheme) (draw cs) (serializedWindowSize ss) (const Nothing) rs

main :: IO ()
main = do
    args <- getArgs

    stateFilePath <- case args of
        (p:_) -> return p
        _ -> usage

    stateBytes <- BSL.readFile stateFilePath
    loadedState <- case A.eitherDecode stateBytes :: Either String SerializedState of
        Left e -> do
            putStrLn $ "Error decoding state file: " <> e
            exitFailure
        Right s -> return s

    vty <- mkVty =<< standardIOConfig

    let cases = bgroup "main"
            [ bench "buildImage" $ nf doBuild loadedState
            , bench "drawImage" $ nfIO $ do
                let result@(_, pic, _, _) = doBuild loadedState
                update vty pic
                return result
            ]
        reportPath = "matterhorn-report.html"
        config = defaultConfig { reportFile = Just reportPath
                               , verbosity = Quiet
                               }

    defaultMainWith config [cases] `finally` shutdown vty

    putStrLn $ "Criterion report written to " <> reportPath
