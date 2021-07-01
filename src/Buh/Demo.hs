{-# LANGUAGE OverloadedStrings #-}

module Buh.Demo
  ( exe
  ) where

import Buh.Internal (CustomDef (..), NextOp (..), ReqBody (..), ResBody (..), UiSt (..), UserCommandHandler,
                     UserEventHandler, UserWorker, addOutgoing, customExe, writeStatus)
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
userCommandHandler uiSt cmd = do
  case T.words cmd of
    ["ping"] -> addOutgoing uiSt (ReqBodyCustom DemoReqBodyPing) $> (NextOpContinue, True)
    ["boom"] -> addOutgoing uiSt (ReqBodyCustom DemoReqBodyBoom) $> (NextOpContinue, True)
    _ -> pure (NextOpContinue, False)

userEventHandler :: UserEventHandler DemoReqBody DemoResBody
userEventHandler (UiSt ref _) cres =
  case cres of
    DemoResBodyPong -> do
      writeStatus ref "Pong"
      pure NextOpContinue

userWorker :: UserWorker DemoReqBody DemoResBody
userWorker _ creq =
  case creq of
    DemoReqBodyPing -> pure (ResBodyCustom DemoResBodyPong)
    DemoReqBodyBoom -> error "Boom!"

customDef :: CustomDef DemoReqBody DemoResBody
customDef = CustomDef userCommandHandler userEventHandler userWorker

exe :: IO ()
exe = customExe customDef
