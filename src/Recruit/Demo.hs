{-# LANGUAGE OverloadedStrings #-}

module Recruit.Demo
  ( exe
  ) where

import Control.Monad.State.Strict (lift, modify')
import Data.Functor (($>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Void (Void)
import GHC.Generics (Generic)
import Recruit.Interface (CommandStatus (..), CustomDef (..), Iface (..), NextOp (..), UserCommandHandler,
                          UserEventHandler, UserParser, UserWorker, ifaceModifyCtx)
import Recruit.Internal (mkExe)
import Recruit.Parser (ParseError, mkUserParser)
import qualified SimpleParser as SP
import System.Random (StdGen, mkStdGen, randomR)
import TextShow (TextShow (..))
import TextShow.Generic (FromGeneric (..))

data DemoReqBody =
    DemoReqBodyPing
  | DemoReqBodyErr
  | DemoReqBodyExc
  | DemoReqBodyRand
  | DemoReqBodyPrint
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric DemoReqBody)

data DemoResBody =
    DemoResBodyPong
  | DemoResBodyRand !Int
  | DemoResBodyPrint
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric DemoResBody)

data DemoErr =
    DemoErrCmd
  | DemoErrParse !(ParseError SP.TextLabel Void)
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric DemoErr)

newtype DemoCtx = DemoCtx { unDemoCtx :: Int }
  deriving stock (Generic)
  deriving newtype (Eq, Show, Num)
  deriving (TextShow) via (FromGeneric DemoCtx)

commandNames :: [(Text, Text)]
commandNames =
  [ ("ping", "test worker responses")
  , ("err", "test worker error handling")
  , ("exc", "test worker exception handling")
  , ("print", "print worker state")
  ]

commandHandler :: UserCommandHandler DemoCtx DemoReqBody
commandHandler iface cmd = do
  case T.words cmd of
    ["ping"] -> ifaceSendReq iface DemoReqBodyPing $> (NextOpContinue, CommandStatusOk)
    ["err"] -> ifaceSendReq iface DemoReqBodyErr $> (NextOpContinue, CommandStatusOk)
    ["exc"] -> ifaceSendReq iface DemoReqBodyExc $> (NextOpContinue, CommandStatusOk)
    ["print"] -> ifaceSendReq iface DemoReqBodyPrint $> (NextOpContinue, CommandStatusOk)
    _ -> pure (NextOpContinue, CommandStatusError)

eventHandler :: UserEventHandler DemoCtx DemoReqBody DemoResBody
eventHandler iface cres =
  case cres of
    DemoResBodyPong -> do
      ifaceWriteStatus iface "Pong"
      pure NextOpContinue
    DemoResBodyRand j -> do
      ifaceWriteStatus iface ("Rand " <> showt j)
      ifaceModifyCtx iface (\(DemoCtx i) -> DemoCtx (i + j))
      pure NextOpContinue
    DemoResBodyPrint -> do
      DemoCtx n <- ifaceGetCtx iface
      ifaceWriteStatus iface ("Value " <> showt n)
      pure NextOpContinue

mkWorker :: IORef StdGen -> UserWorker DemoErr DemoReqBody DemoResBody
mkWorker ioGen _ creq =
  case creq of
    DemoReqBodyPing -> pure (Right DemoResBodyPong)
    DemoReqBodyErr -> pure (Left DemoErrCmd)
    DemoReqBodyExc -> error "Boom!"
    DemoReqBodyRand -> do
      j <- atomicModifyIORef' ioGen (swap . randomR (-1, 1))
      pure (Right (DemoResBodyRand j))
    DemoReqBodyPrint -> pure (Right DemoResBodyPrint)

data Directive =
    DirectiveInc
  | DirectiveDec
  | DirectiveRand
  | DirectivePrint
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric Directive)

parser :: UserParser DemoErr DemoCtx DemoReqBody
parser = mkUserParser DemoErrParse $ do
  SP.spaceParser
  dir <- SP.lookAheadSimple SP.anyToken (fail "failed to match directive")
    [ ('i', SP.matchChunk "inc" $> DirectiveInc)
    , ('d', SP.matchChunk "dec" $> DirectiveDec)
    , ('r', SP.matchChunk "rand" $> DirectiveRand)
    , ('p', SP.matchChunk "print" $> DirectiveRand)
    ]
  SP.spaceParser
  _ <- SP.matchToken '.'
  case dir of
    DirectiveInc -> do
      lift (modify' (\(DemoCtx n) -> DemoCtx (n + 1)))
      pure Nothing
    DirectiveDec -> do
      lift (modify' (\(DemoCtx n) -> DemoCtx (n - 1)))
      pure Nothing
    DirectiveRand -> pure (Just DemoReqBodyRand)
    DirectivePrint -> pure (Just DemoReqBodyPrint)

mkCustomDef :: IORef StdGen -> CustomDef DemoErr DemoCtx DemoReqBody DemoResBody
mkCustomDef ioGen =
  let worker = mkWorker ioGen
      emptyCtx = DemoCtx 0
  in CustomDef commandNames commandHandler eventHandler worker parser emptyCtx

exe :: IO ()
exe = do
  ioGen <- newIORef (mkStdGen 42)
  let customDef = mkCustomDef ioGen
  mkExe customDef
