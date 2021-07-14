module Recruit.Interface where

import Control.Concurrent.STM (STM)
import Data.Text (Text)
import GHC.Generics (Generic)
import LittleLogger.Manual (Severity, SimpleLogAction)
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

data NextOp =
    NextOpHalt
  | NextOpContinue
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric NextOp)

data CommandStatus =
    CommandStatusOk
  | CommandStatusError
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric CommandStatus)

newtype Pos = Pos { unPos :: Int }
  deriving stock (Generic)
  deriving newtype (Eq, Show, Ord, Enum)
  deriving (TextShow) via (FromGeneric Pos)

data Span = Span
  { spanStart :: !Pos
  , spanEnd :: !Pos
  } deriving stock (Eq, Show, Generic)
    deriving (TextShow) via (FromGeneric Span)

data Result err ctx req =
    ResultEmpty
  | ResultForward !Pos !ctx !(Maybe req)
  | ResultError !Span !err
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric (Result err ctx req))

data Iface ctx req = Iface
  { ifaceSendReq :: !(req -> STM ())
  , ifaceSendLog :: !(Severity -> Text -> STM ())
  , ifaceWriteStatus :: !(Text -> STM ())
  , ifaceGetCtx :: !(STM ctx)
  , ifacePutCtx :: !(ctx -> STM ())
  }

ifaceModifyCtx :: Iface ctx req -> (ctx -> ctx) -> STM ()
ifaceModifyCtx iface f = do
  ctx <- ifaceGetCtx iface
  ifacePutCtx iface (f ctx)

type CommandNames = [(Text, Text)]

type UserCommandHandler ctx req = Iface ctx req -> Text -> STM (NextOp, CommandStatus)

type UserEventHandler ctx req res = Iface ctx req -> res -> STM NextOp

type UserWorker err req res = SimpleLogAction -> req -> IO (Either err res)

type UserParser err ctx req = ctx -> Text -> Result err ctx req

data CustomDef err ctx req res = CustomDef
  { customCommandNames :: !CommandNames
  , customCommandHandler :: !(UserCommandHandler ctx req)
  , customEventHandler :: !(UserEventHandler ctx req res)
  , customWorker :: !(UserWorker err req res)
  , customParser :: !(UserParser err ctx req)
  , customEmptyCtx :: !ctx
  }

type CustomConstraints err ctx req res = (TextShow err, TextShow req)
