module Buh.Etc
  (
  ) where

import Data.Either (isLeft)
import Data.Text (Text)

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
