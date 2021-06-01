{-# LANGUAGE OverloadedStrings #-}

{-
See https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst
https://github.com/jtdaugherty/brick/blob/master/programs/CustomEventDemo.hs
-}
module Buh where

import Brick (App (..), BrickEvent (..), EventM, Next, Widget, attrMap, continue, defaultMain, halt, showFirstCursor, txt)
import qualified Graphics.Vty as V
import Data.Either (isLeft)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (die)

newtype Pos = Pos Int deriving (Eq, Show, Ord)

data Span = Span
  { spanStart :: !Pos
  , spanEnd :: !Pos
  } deriving (Eq, Show)

data Elem e a = Elem
  { elemSpan :: !Span
  , elemContents :: !Text
  , elemParsed :: !(Either e a)
  } deriving (Eq, Show)

data Buffer e a = Buffer
  { bufferUp :: !(Maybe (Buffer e a))
  , bufferElem :: !(Elem e a)
  , bufferTail :: !Text
  } deriving (Eq, Show)

type Parser e a = Text -> Maybe (Elem e a, Text)

bufferDown :: Parser e a -> Buffer e a -> Maybe (Buffer e a)
bufferDown p b = fmap (uncurry (Buffer (Just b))) (p (bufferTail b))

bufferIsOk :: Buffer e a -> Bool
bufferIsOk = isLeft . elemParsed . bufferElem

bufferBottom :: Parser e a -> Buffer e a -> Buffer e a
bufferBottom p b =
  if bufferIsOk b
    then maybe b (bufferBottom p) (bufferDown p b)
    else b

bufferTop :: Buffer e a -> Buffer e a
bufferTop b = maybe b bufferTop (bufferUp b)

data St = St
  { stFilename :: !(Maybe FilePath)
  , stContents :: !Text
  } deriving (Eq, Show)

initialize :: Maybe FilePath -> IO St
initialize mfn = do
  contents <- maybe (pure "") TIO.readFile mfn
  pure (St mfn contents)

draw :: St -> [Widget ()]
draw st = [txt (stContents st)]

handle :: St -> BrickEvent () Void -> EventM () (Next St)
handle st e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt st
    _ -> continue st

app :: App St Void ()
app = App
  { appDraw = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handle
  , appStartEvent = pure
  , appAttrMap = const (attrMap V.defAttr [])
  }

exe :: IO ()
exe = do
  args <- getArgs
  mfn <- case args of
    [] -> pure Nothing
    [fn] -> pure (Just fn)
    _ -> die "Use: buffer.hs [filename]"
  print "Initializing state"
  initSt <- initialize mfn
  putStrLn "Starting UI"
  _ <- defaultMain app initSt
  putStrLn "Stopped UI"
