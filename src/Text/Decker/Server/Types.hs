{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Types where

import Control.Concurrent
import Control.Lens
import qualified Data.Set as Set
import Data.Time
import Network.WebSockets
import Relude
import Web.Scotty.Trans

-- | Clients are identified by integer ids
type Client = (Int, Connection)

type AppScottyM = ScottyT Text (ReaderT Server IO)
type AppActionM = ActionT Text (ReaderT Server IO)

data ServerState 
  = ServerState {
  clients :: [Client], 
  observed :: Set.Set FilePath
  }

data VideoOperation
  = Replace FilePath FilePath
  | Append FilePath FilePath
  deriving (Show)

data ActionMsg
  = ServerExit String
  | FileChanged UTCTime FilePath
  | UploadComplete VideoOperation
  deriving (Show)

data Server = Server
  { _threadId :: ThreadId,
    _serverState :: TVar ServerState
  }
  deriving (Eq)

makeLenses ''Server
