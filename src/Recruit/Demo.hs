{-# LANGUAGE OverloadedStrings #-}

module Recruit.Demo
  ( exe
  ) where

import Data.Functor (($>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Recruit.Interface (CommandStatus (..), CustomDef (..), Iface (..), NextOp (..), Result (..), UserCommandHandler,
                          UserEventHandler, UserParser, UserWorker, ifaceModifyCtx)
import Recruit.Internal (mkExe)
import SimpleParser (ErrorExplanation, Offset (Offset), OffsetStream (..), ParseResult (..), ParseSuccess (..), Parser,
                     TextLabel, newOffsetStream, parseErrorNarrowestSpan, runParser)
import System.Random (StdGen, mkStdGen, randomR)
import TextShow (TextShow (..))
import TextShow.Generic (FromGeneric (..))

data DemoReqBody =
    DemoReqBodyPing
  | DemoReqBodyErr
  | DemoReqBodyExc
  | DemoReqBodyRand
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric DemoReqBody)

data DemoResBody =
    DemoResBodyPong
  | DemoResBodyRand !Int
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric DemoResBody)

data DemoErr =
    DemoErrCmd
  | DemoErrParse !ErrorExplanation
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
  ]

commandHandler :: UserCommandHandler DemoCtx DemoReqBody
commandHandler iface cmd = do
  case T.words cmd of
    ["ping"] -> ifaceSendReq iface DemoReqBodyPing $> (NextOpContinue, CommandStatusOk)
    ["err"] -> ifaceSendReq iface DemoReqBodyErr $> (NextOpContinue, CommandStatusOk)
    ["exc"] -> ifaceSendReq iface DemoReqBodyExc $> (NextOpContinue, CommandStatusOk)
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

mkWorker :: IORef StdGen -> UserWorker DemoErr DemoReqBody DemoResBody
mkWorker ioGen _ creq =
  case creq of
    DemoReqBodyPing -> pure (Right DemoResBodyPong)
    DemoReqBodyErr -> pure (Left DemoErrCmd)
    DemoReqBodyExc -> error "Boom!"
    DemoReqBodyRand -> do
      j <- atomicModifyIORef' ioGen (swap . randomR (-1, 1))
      pure (Right (DemoResBodyRand j))

data Directive =
    DirectiveInc
  | DirectiveDec
  | DirectiveRand
  | DirectivePrint
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric Directive)

type P a = Parser TextLabel (OffsetStream Text) Text a

parseTxt :: P Directive
parseTxt = undefined

parser :: UserParser DemoErr DemoCtx DemoReqBody
parser ctx@(DemoCtx i) txt =
  case runParser parseTxt (newOffsetStream txt) of
    Nothing -> ResultEmpty
    Just (ParseResultError (perr :<|| _)) ->
      -- Just take the first error
      let (_, espan) = parseErrorNarrowestSpan perr
      in undefined
    Just (ParseResultSuccess (ParseSuccess (OffsetStream (Offset o) _) dir)) ->
      undefined

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
