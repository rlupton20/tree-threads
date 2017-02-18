# Tree Threads

Tree threads are a kind of threaded reader monad, with the property that if branches die, that failure doesn't propagate to the parents, but does propagate to any child threads. This makes them well suited to compartmentalizing process that may fail "locally", and can be handled cleanly, without needing to bring down an entire application.

## Example

```
import Control.Concurrent.TreeThreads

example :: IO ()
example = root `sproutOn` 42

root :: TreeThread Int ()
root = do
  e <- environment -- e is 42
  sprout branch
  -- Carry on doing something - if the spawned branch fails
  -- it won't destroy root.
  ...

branch :: TreeThread Int ()
branch = do
  e <- environment -- e is also 42
  -- do something useful
```