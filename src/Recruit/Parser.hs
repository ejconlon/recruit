{-# LANGUAGE OverloadedStrings #-}

-- | Wrapper for SimpleParser
module Recruit.Parser where

import Control.Monad.State.Strict (State, runState)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Recruit.Interface (Pos (..), Result (..), Span (..), UserParser)
import Recruit.Orphans ()
import qualified SimpleParser as SP
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

type Parser lab err ctx a = SP.ParserT lab (SP.OffsetStream Text) err (State ctx) a

data ParseError lab err = ParseError
  { parseErrorLabels :: !(Seq lab)
  , parseErrorCause :: !(SP.CompoundError (SP.OffsetStream Text) err)
  } deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric (ParseError lab err))

mkUserParser :: (ParseError lab err -> err') -> Parser lab err ctx (Maybe req) -> UserParser err' ctx req
mkUserParser mapErr parser ctx txt =
  let (mres, ctx') = runState (SP.runParserT parser (SP.newOffsetStream txt)) ctx
  in case mres of
    Nothing -> ResultEmpty
    Just (SP.ParseResultError errs) ->
      case SP.matchSoleParseError errs of
        Nothing ->
          -- We don't support multiple parse errors - use 'lookAheadMatch' instead of arbitrary branching
          -- to construct the parser with look-ahead in the right places.
          ResultFail "multiple errors not supported"
        Just err ->
          let (_, SP.Span (SP.Offset startChar) (SP.Offset endChar)) = SP.parseErrorNarrowestSpan err
              sp = Span (Pos startChar) (Pos endChar)
              labs = SP.parseErrorLabels err
          in ResultError sp (mapErr (ParseError labs (SP.peError err)))
    Just (SP.ParseResultSuccess (SP.ParseSuccess s mreq)) ->
      let SP.Offset endChar = SP.streamViewPos s
      in ResultForward (Pos endChar) ctx' mreq
