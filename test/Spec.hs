import Test.Framework
import Test.Framework.Providers.HUnit

import TreeThreadsTest (managerTest)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ managerTest ]
