module Buh.Log
  ( Log (..)
  , newLog
  , pushLog
  , peekLog
  , markLog
  ) where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

-- TODO maintain seen/unseen as lists not single new flag
data Log a = Log
  { logMax :: !Int
  , logElems :: !(Seq a)
  , logNew :: !Bool
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

newLog :: Int -> Log a
newLog i = Log i Seq.empty False

pushLog :: Log a -> a -> Log a
pushLog (Log m e _) v = Log m f True where
  f = v :<| g
  g = case e of
    Empty -> e
    start :|> _ -> if Seq.length e >= m then start else e

peekLog :: Log a -> Maybe (a, Bool)
peekLog (Log _ e n) =
  case e of
    hd :<| _ -> Just (hd, n)
    Empty -> Nothing

markLog :: Log a -> Log a
markLog (Log m e _) = Log m e False
