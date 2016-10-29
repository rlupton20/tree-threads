{-# LANGUAGE ExistentialQuantification, RankNTypes, RecordWildCards, DeriveDataTypeable #-}
module Control.Concurrent.TreeThreads.Internal where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async ( Async, async, cancel
                                , waitCatch, waitCatchSTM )
import Control.Concurrent.STM ( STM, TVar, atomically, retry, orElse, newTVarIO
                              , readTVar, writeTVar, swapTVar, modifyTVar' )

import Control.Exception ( Exception, throwTo, throwIO, try, catch, mask
                         , SomeException, AsyncException(ThreadKilled)
                         , fromException )

import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)

import Data.Typeable (Typeable)

  
data Branch = Branch { process :: Async () }
type BranchRecord = TVar (Maybe [Branch])
data TreeCtl = TreeCtl { branches :: BranchRecord }
type TreeThread a = ReaderT a (ReaderT TreeCtl IO)

data PruneCrash = PruneCrash deriving (Eq, Show, Typeable)
instance Exception PruneCrash

-- |sproutOn starts a tree-thread process with an empty list of
-- branches, and launches a pruning thread, which removes
-- completed branches from a tracking list.
sproutOn :: TreeThread a () -> a -> IO ()
sproutOn tt env = do
  branches <- newTVarIO $ Just []
  tid <- myThreadId
  
  mask $ \restore -> do
    pruner <- async $ pruneLoop restore tid branches
    r <- try (restore (runReaderT (runReaderT tt env) $ (TreeCtl branches)))
    case r of
      -- If our tree-thread has crashed with some exception, then we pass the
      -- exception to a handler to do the cleanup.
      Left e -> handleException e branches pruner
      -- Otherwise, our tree-thread ran successfully, and there is nothing left
      -- to do, so cleanup any remaining child threads (branches).
      Right _ -> do
        cancel pruner
        killBranches branches
        waitCatch pruner
        return ()

  where
    
    pruneLoop :: (forall a. IO a -> IO a) ->
                ThreadId ->
                BranchRecord ->
                IO ()
    pruneLoop restore parentID branches = let tk = ThreadKilled in
      restore (forever $ prune branches) `catch`
        (\e -> if e == tk then throwIO e else do
            throwTo parentID PruneCrash
            throwIO e)


    -- handleException has the task of cleaning up. There are two cases:
    -- 1) either our pruning thread crashed and sent us an exception, in
    --    which case pruner is already dealing with an exception and we
    --    just need to wait for it to do its cleanup.
    -- 2) something went wrong in the tree-thread itself, and pruner doesn't
    --    know about this, so we need to cancel the pruner thread manually,
    --    and wait for it to do cleanup.
    -- We deal with these two cases separately. In both cases, the branches
    -- need cleaning up.
    handleException :: SomeException ->
                       BranchRecord ->
                       Async () ->
                       IO ()
    handleException e branches pruner = do 
      killBranches branches
      let ex = fromException e :: Maybe PruneCrash
      case ex of
        Just _ -> {- The exception was a PruneCrash -} waitCatch pruner >> return ()
        Nothing -> cancel pruner >> waitCatch pruner >> return ()
      throwIO e
    
    killBranches :: BranchRecord -> IO ()
    killBranches br = do
      branches <- getKillList br
      sequence $ map kill branches
      sequence $ map (waitCatch.process) branches
      return ()
      
    getKillList :: BranchRecord -> IO [Branch]
    getKillList br = atomically $ do
      branches <- swapTVar br Nothing
      case branches of
        Just targets -> return targets
        Nothing -> {- Already killed -} return []

    kill :: Branch -> IO ()
    kill Branch{..} = cancel process

-- |prune takes our tracked Branches, and returns a list of
-- completed branches, leaving behind only those that are
-- still running.
prune :: BranchRecord -> IO [Branch]
prune br = atomically $ do
    branches <- readTVar br
    maybe (return []) tidy branches
    where
      tidy :: [Branch] -> STM [Branch]
      tidy branches = do
        (done,working) <- divideBranches branches
        writeTVar br (Just working)
        return done


-- |divideBranches takes a list of Bracnches and returns two lists:
-- one a list of completed branches, and the other a list of those
-- still running. Ordering of lists is not guaranteed to be preserved.
-- Will block if nothing has finished running.
divideBranches :: [Branch] -> STM ([Branch],[Branch])
divideBranches branches = go [] [] branches
  where
    -- This is really a right fold, but expressing it that way is
    -- opaque. This recursive worker function works along the list
    -- of branches and divides them into completed and retries.
    go [] _ [] = retry
    go cs rs [] = return (cs,rs)
    go cs rs (s:ss) = (tryDecide s cs rs ss) `orElse` go cs (s:rs) ss

    -- tryDecide looks to see if the branch b has finished,
    -- otherwise it does an STM retry. Note that waitCatchSTM contains
    -- a retry. If the branch has finished, it is added to the
    -- completed list.
    tryDecide b cs rs ss = do
      _ <- waitCatchSTM (process b)
      go (b:cs) rs ss


-- |sprout creates a new tree-thread in a new thread, that is,
-- sprout creates a new branch to run a tree-thread in. If the new branch
-- crashes, it is caught by prune, and the exception is not propogated.
-- Each sprouted branch has its own new collection of sub-branches,
-- and its own pruning thread. This is intended to create independent
-- branches --- if the parent branch wants a result back from the
-- branch then another method should be used, e.g. Async or a
-- thread safe variable.
-- The branch is of course responsible for leaving resources in a
-- consistent state in the event of an exception.
sprout :: TreeThread a () -> TreeThread a Branch
sprout tt = do
  env <- environment
  br <- getBranchRecord
  liftIO $ mask $ \restore -> do
    a <- async (restore $ tt `sproutOn` env)
    let b = Branch a
    br `track` b
    restore (return b)
    where
      track :: BranchRecord -> Branch -> IO ()
      track br b = atomically $ modifyTVar' br $ fmap (b:)
  {- There is some subtlty to the operation here. We want to be
     sure that there is no way we can lose track of the newly
     spawned thread.

     Note that the contents of the BranchRecord TVar can only
     be set to Nothing, if an exception has been hit or thrown to
     the TreeThread and execution has moved to the handler. Exceptions
     are masked, so this can only happen if an exception is thrown
     on a blocked operation. The documentation Control.Exception
     guarantees that STM transactions which don't use retry are
     guaranteed to be uniterruptible. In sum this means that once
     the new Async has been created, the BranchRecord TVar must
     contain data of form Just [Branch] and hence our new
     branch is tracked before the next exception strikes.

     In other words, there is no chance of leaking the new thread.

     If understanding is required, it is worth simultaneously checking
     how sproutOn behaves, and how it handles exceptions. -}



-- |getBranchRecord provides the current running list of branches of a
-- tree-thread. Users of the TreeThread monad should not be concerned with
-- these, so this function is not exported, and only used internally.
getBranchRecord :: TreeThread a BranchRecord
getBranchRecord = lift (asks branches)

-- |fromEnvironment provides the result of applying a passed function
-- to the current environment
withEnvironment :: (a -> b) -> TreeThread a b
withEnvironment = asks

-- |environment provides the current environment.
environment :: TreeThread a a
environment = ask
