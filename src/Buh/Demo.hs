{-# LANGUAGE OverloadedStrings #-}

module Buh.Demo
  ( exe
  ) where

import Buh.Interface (CustomDef (..), Iface (..), NextOp (..), ReqBody (..), ResBody (..), UserCommandHandler,
                      UserEventHandler, UserWorker)
import Buh.Internal (mkExe)
import Data.Functor (($>))
import qualified Data.Text as T

data DemoReqBody =
    DemoReqBodyPing
  | DemoReqBodyBoom
  deriving stock (Eq, Show)

data DemoResBody =
    DemoResBodyPong
  deriving stock (Eq, Show)

userCommandHandler :: UserCommandHandler DemoReqBody
userCommandHandler iface cmd = do
  case T.words cmd of
    ["ping"] -> ifaceSend iface (ReqBodyCustom DemoReqBodyPing) $> (NextOpContinue, True)
    ["boom"] -> ifaceSend iface (ReqBodyCustom DemoReqBodyBoom) $> (NextOpContinue, True)
    _ -> pure (NextOpContinue, False)

userEventHandler :: UserEventHandler DemoReqBody DemoResBody
userEventHandler iface cres =
  case cres of
    DemoResBodyPong -> do
      ifaceWriteStatus iface "Pong"
      pure NextOpContinue

userWorker :: UserWorker DemoReqBody DemoResBody
userWorker _ creq =
  case creq of
    DemoReqBodyPing -> pure (ResBodyCustom DemoResBodyPong)
    DemoReqBodyBoom -> error "Boom!"

customDef :: CustomDef DemoReqBody DemoResBody
customDef = CustomDef userCommandHandler userEventHandler userWorker

exe :: IO ()
exe = mkExe customDef
