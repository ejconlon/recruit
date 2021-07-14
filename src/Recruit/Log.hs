{-# LANGUAGE OverloadedStrings #-}

module Recruit.Log
  ( Log (..)
  , newLog
  , pushLog
  , peekLog
  , markLog
  , textLog
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import GHC.Generics (Generic)
import Recruit.Orphans ()
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

-- TODO maintain seen/unseen as lists not single new flag
data Log a = Log
  { logMax :: !Int
  , logElems :: !(Seq a)
  , logNew :: !Bool
  } deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
    deriving (TextShow) via (FromGeneric (Log a))

newLog :: Int -> Log a
newLog i = Log i Seq.empty False

pushLog :: Log a -> a -> Log a
pushLog (Log m e _) v = Log m f True where
  f = g :|> v
  g = case e of
    Empty -> e
    _ :<| rest -> if Seq.length e >= m then rest else e

peekLog :: Log a -> Maybe (a, Bool)
peekLog (Log _ e n) =
  case e of
    _ :|> las -> Just (las, n)
    Empty -> Nothing

markLog :: Log a -> Log a
markLog (Log m e _) = Log m e False

textLog :: Log Text -> [Text]
textLog (Log _ elems _) = toList elems
