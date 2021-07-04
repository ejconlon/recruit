{-# LANGUAGE OverloadedStrings #-}

module Buh.Demo
  ( exe
  ) where

import Buh.Interface (CommandStatus (..), CustomDef (..), Iface (..), NextOp (..), Result (..), UserCommandHandler,
                      UserEventHandler, UserParser, UserWorker)
import Buh.Internal (mkExe)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

data DemoReqBody =
    DemoReqBodyPing
  | DemoReqBodyErr
  | DemoReqBodyExc
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric DemoReqBody)

data DemoResBody =
    DemoResBodyPong
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric DemoResBody)

data DemoErr = DemoErr
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric DemoErr)

type DemoCtx = ()

userCommandNames :: [(Text, Text)]
userCommandNames =
  [ ("ping", "test worker responses")
  , ("err", "test worker error handling")
  , ("exc", "test worker exception handling")
  ]

userCommandHandler :: UserCommandHandler DemoCtx DemoReqBody
userCommandHandler iface cmd = do
  case T.words cmd of
    ["ping"] -> ifaceSendReq iface DemoReqBodyPing $> (NextOpContinue, CommandStatusOk)
    ["err"] -> ifaceSendReq iface DemoReqBodyErr $> (NextOpContinue, CommandStatusOk)
    ["exc"] -> ifaceSendReq iface DemoReqBodyExc $> (NextOpContinue, CommandStatusOk)
    _ -> pure (NextOpContinue, CommandStatusError)

userEventHandler :: UserEventHandler DemoCtx DemoReqBody DemoResBody
userEventHandler iface cres =
  case cres of
    DemoResBodyPong -> do
      ifaceWriteStatus iface "Pong"
      pure NextOpContinue

userWorker :: UserWorker DemoErr DemoReqBody DemoResBody
userWorker _ creq =
  case creq of
    DemoReqBodyPing -> pure (Right DemoResBodyPong)
    DemoReqBodyErr -> pure (Left DemoErr)
    DemoReqBodyExc -> error "Boom!"

userParser :: UserParser DemoErr DemoCtx DemoReqBody
userParser _ _ = ResultEmpty

customDef :: CustomDef DemoErr DemoCtx DemoReqBody DemoResBody
customDef = CustomDef userCommandNames userCommandHandler userEventHandler userWorker userParser ()

exe :: IO ()
exe = mkExe customDef
