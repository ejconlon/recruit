module Buh.Interface where

import Control.Concurrent.STM (STM)
import Data.Text (Text)
import LittleLogger.Manual (Severity, SimpleLogAction)

newtype ReqId = ReqId { unReqId :: Int }
  deriving newtype (Eq, Show, Enum, Ord, Num)

data ReqBody req =
    ReqBodyCustom !req
  | ReqBodyOpen !FilePath
  | ReqBodyLog !Severity !Text
  deriving stock (Eq, Show)

data ResBody res =
    ResBodyCustom !res
  | ResBodyOpened !FilePath !Text
  | ResBodyNotFound !FilePath
  | ResBodyErr !Text
  | ResBodyLog
  deriving stock (Eq, Show)

data NextOp = NextOpHalt | NextOpContinue
  deriving (Eq, Show)

data Iface req = Iface
  { ifaceSend :: !(ReqBody req -> STM ReqId)
  , ifaceWriteStatus :: !(Text -> STM ())
  }

type UserCommandHandler req = Iface req -> Text -> STM (NextOp, Bool)

type UserEventHandler req res = Iface req -> res -> STM NextOp

type UserWorker req res = SimpleLogAction -> req -> IO (ResBody res)

data CustomDef req res = CustomDef
  { customCommandNames :: ![(Text, Text)]
  , customCommandHandler :: !(UserCommandHandler req)
  , customEventHandler :: !(UserEventHandler req res)
  , customWorker :: !(UserWorker req res)
  }
