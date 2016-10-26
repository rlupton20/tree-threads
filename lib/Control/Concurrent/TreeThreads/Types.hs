module Control.Concurrent.TreeThreads.Types where

import Control.Concurrent.Async ( Async )
import Control.Concurrent.STM.TVar ( TVar )

import Control.Monad.Trans.Reader ( ReaderT )


data Submanager = Submanager { process :: Async () }
type SubmanagerLog = TVar (Maybe [Submanager])
data ManageCtl = ManageCtl { submanagers :: SubmanagerLog }
type Managed a = ReaderT a (ReaderT ManageCtl IO)
