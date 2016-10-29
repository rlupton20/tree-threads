import Test.Framework
import Test.Framework.Providers.HUnit

import TreeThreadsTest ( treeThreadTests )

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ treeThreadTests ]
