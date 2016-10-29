module TreeThreadsTest
( treeThreadTests ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Control.Concurrent (threadDelay)
import System.Timeout ( timeout )
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVarIO, takeTMVar, putTMVar )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, getMaskingState, MaskingState(..))

import Control.Concurrent.TreeThreads.Internal

testTimeout :: Int
testTimeout = 1000000

  
treeThreadTests :: TF.Test
treeThreadTests = testGroup "TreeThread tests" . hUnitTestToTests $
  HU.TestList [ basicFunctionalTests ]

  
basicFunctionalTests :: HU.Test
basicFunctionalTests = HU.TestLabel "Basic functional tests" $
  HU.TestList [ testSprout
              , testExceptionsDontPropagateDown
              , testSproutOnMaskingState
              , testSproutForksToUnmaskedState
              , testBranchIsTrackedCorrectly ]


testSprout :: HU.Test
testSprout = "sprout: can spawn a thread which does some work using the environment (1s timeout)" ~: do
  tmvar <- newEmptyTMVarIO
  getUnit `sproutOn` tmvar
  where
    getUnit :: TreeThread (TMVar ()) ()
    getUnit = do
      sprout putUnit
      t <- environment
      liftIO $ do
        val <- timeout testTimeout . atomically $ takeTMVar t
        Just () @=? val

    putUnit :: TreeThread (TMVar ()) ()
    putUnit = do
      t <- environment
      liftIO . atomically $ putTMVar t ()
    

testExceptionsDontPropagateDown :: HU.Test
testExceptionsDontPropagateDown = "tree-threads: check exceptions don't propagate downwards (takes 1 second)" ~: do
  tmvar <- newEmptyTMVarIO
  stayAlive `sproutOn` tmvar
  where
    stayAlive :: TreeThread (TMVar ()) ()
    stayAlive = do
      sprout kamikaze
      t <- environment
      liftIO $ do
        val <- timeout testTimeout . atomically $ takeTMVar t
        Nothing @=? val

    kamikaze :: TreeThread (TMVar ()) ()
    kamikaze = do
      t <- environment
      liftIO $ do
        True @=? False
        atomically $ putTMVar t ()


testSproutOnMaskingState :: HU.Test
testSproutOnMaskingState = "sproutOn: check the TreeThread executes unmasked (when executed unmasked)" ~:
  (liftIO $ getMaskingState >>= (@=? Unmasked)) `sproutOn` ()


testSproutForksToUnmaskedState :: HU.Test
testSproutForksToUnmaskedState = "sprout: check the sprout-ed TreeThread executes unmasked (when called unmasked)" ~: do
  tmvar <- newEmptyTMVarIO
  askMasking `sproutOn` tmvar
  where
    askMasking :: TreeThread (TMVar MaskingState) ()
    askMasking = do
      sprout reporter
      t <- environment
      liftIO $ do
        ms <- timeout testTimeout . atomically $ takeTMVar t
        Just Unmasked @=? ms

    reporter :: TreeThread (TMVar MaskingState) ()
    reporter = do
      t <- environment
      liftIO $ getMaskingState >>= atomically . putTMVar t
                              

testBranchIsTrackedCorrectly :: HU.Test
testBranchIsTrackedCorrectly = "sprout: check new branches are tracked and cleaned up correctly (normal execution)" ~: do
  tmvar <- newEmptyTMVarIO
  tracker `sproutOn` tmvar
  where
    tracker :: TreeThread (TMVar ()) ()
    tracker = do
      getBranches >>= assertThereAre 0
      sprout waitForCheck
      getBranches >>= assertThereAre 1
      tmvar <- environment
      liftIO $ do
        atomically $ putTMVar tmvar ()
        threadDelay testTimeout
      getBranches >>= assertThereAre 0

    assertThereAre :: Int -> Maybe [Branch] -> TreeThread a ()
    assertThereAre n = liftIO . (Just n @=?) . fmap length

    waitForCheck :: TreeThread (TMVar ()) ()
    waitForCheck = do
      tmvar <- environment
      liftIO . atomically $ takeTMVar tmvar



-- |getBranches gets the current list of branches
getBranches :: TreeThread a (Maybe [Branch])
getBranches = do
  br <- getBranchRecord
  liftIO $ readTVarIO br
