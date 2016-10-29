module TreeThreadsTest
( managerTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, getMaskingState, MaskingState(..))

import Control.Concurrent.TreeThreads.Internal

managerTest :: TF.Test
managerTest = testGroup "Manager.hs tests" $ hUnitTestToTests $ HU.TestList []

managerUnitTests :: HU.Test
managerUnitTests = HU.TestLabel "Basic manager unit tests" $ HU.TestList [] 
