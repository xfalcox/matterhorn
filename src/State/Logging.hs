module State.Logging
  ( startLogging
  , stopLogging
  , logSnapshot
  , getLogDestination
  )
where

import           Prelude ()
import           Prelude.MH
import           Lens.Micro.Platform (to)

import           Types


startLogging :: FilePath -> MH ()
startLogging path = do
    mgr <- use (csResources.crMutable.to mutLogManager)
    liftIO $ startLoggingToFile mgr path

stopLogging :: MH ()
stopLogging = do
    mgr <- use (csResources.crMutable.to mutLogManager)
    liftIO $ stopLoggingToFile mgr

logSnapshot :: FilePath -> MH ()
logSnapshot path = do
    mgr <- use (csResources.crMutable.to mutLogManager)
    liftIO $ requestLogSnapshot mgr path

getLogDestination :: MH ()
getLogDestination = do
    mgr <- use (csResources.crMutable.to mutLogManager)
    liftIO $ requestLogDestination mgr
