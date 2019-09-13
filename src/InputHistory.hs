{-# LANGUAGE TemplateHaskell #-}
module InputHistory
  ( InputHistory
  , newHistory
  , readHistory
  , writeHistory
  , addHistoryEntry
  , getHistoryEntry
  , removeChannelHistory
  )
where

import           Prelude ()
import           Prelude.MH

import           Control.Monad.Trans.Except
import           Data.Hashable ( Hashable )
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (.~), (^?), (%~), at, ix, makeLenses )
import           System.Directory ( createDirectoryIfMissing )
import           System.FilePath ( dropFileName )
import qualified System.IO.Strict as S
import qualified System.Posix.Files as P
import qualified System.Posix.Types as P

import           FilePaths
import           IOUtil


data InputHistory a =
    InputHistory { _historyEntries :: HashMap a (V.Vector Text)
                 }
                 deriving (Show)

makeLenses ''InputHistory

newHistory :: (Hashable a, Eq a) => InputHistory a
newHistory = InputHistory mempty

removeChannelHistory :: (Eq a, Hashable a) => a -> InputHistory a -> InputHistory a
removeChannelHistory k ih = ih & historyEntries.at k .~ Nothing

historyFileMode :: P.FileMode
historyFileMode = P.unionFileModes P.ownerReadMode P.ownerWriteMode

writeHistory :: (Show a) => InputHistory a -> IO ()
writeHistory ih = do
    historyFile <- historyFilePath
    createDirectoryIfMissing True $ dropFileName historyFile
    let entries = (\(k, z) -> (k, V.toList z)) <$>
                  (HM.toList $ ih^.historyEntries)
    writeFile historyFile $ show entries
    P.setFileMode historyFile historyFileMode

readHistory :: (Hashable a, Eq a, Read a) => IO (Either String (InputHistory a))
readHistory = runExceptT $ do
    contents <- convertIOException (S.readFile =<< historyFilePath)
    case reads contents of
        [(val, "")] -> do
            let entries = (\(k, es) -> (k, V.fromList es)) <$> val
            return $ InputHistory $ HM.fromList entries
        _ -> throwE "Failed to parse history file"

addHistoryEntry :: (Hashable a, Eq a) => Text -> a -> InputHistory a -> InputHistory a
addHistoryEntry e k ih = ih & historyEntries.at k %~ insertEntry
    where
    insertEntry Nothing  = Just $ V.singleton e
    insertEntry (Just v) =
      Just $ V.cons e (V.filter (/= e) v)

getHistoryEntry :: (Hashable a, Eq a) => a -> Int -> InputHistory a -> Maybe Text
getHistoryEntry k i ih = do
    es <- ih^.historyEntries.at k
    es ^? ix i
