module Control.Concurrent.TreeThreads.Types where

import Control.Concurrent.Async ( Async )
import Control.Concurrent.STM.TVar ( TVar )

import Control.Monad.Trans.Reader ( ReaderT )

import Routing.RoutingTable
import Command.Types

data Environment = Environment { routingTable :: RoutingTable
                               , commandQueue :: CommandQueue }

data Submanager = Submanager { process :: Async () }
type SubmanagerLog = TVar (Maybe [Submanager])
data ManageCtl = ManageCtl { submanagers :: SubmanagerLog }
type Manager = ReaderT Environment (ReaderT ManageCtl IO)
