module Recruit.Completes
  ( Trie
  , mkTrie
  , Completes
  , mkCompletes
  , mkEmptyCompletes
  , readCompletes
  , advanceCompletes
  ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import GHC.Generics (Generic)
import Recruit.Orphans ()
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

mkTrie :: [Text] -> Trie
mkTrie = Trie.fromList . fmap T.unpack

data Completes = Completes
  { compQuery :: !Text
  , compMatches :: !(Seq Text)
  , compIndex :: !(Maybe Int)
  } deriving stock (Eq, Show, Generic)
    deriving (TextShow) via (FromGeneric Completes)

mkCompletes :: Trie -> Text -> Completes
mkCompletes trie query =
  let sq = T.unpack query
      suffs = Trie.possibleSuffixes sq trie
      idx = if null suffs then Nothing else Just 0
  in Completes query (Seq.fromList [T.pack (sq ++ x) | x <- suffs]) idx

mkEmptyCompletes :: [Text] -> Completes
mkEmptyCompletes matches = Completes T.empty (Seq.fromList matches) (if null matches then Nothing else Just 0)

readCompletes :: Completes -> Text
readCompletes (Completes q ms mi) =
  case mi of
    Nothing -> q
    Just i -> Seq.index ms i

advanceCompletes :: Completes -> Completes
advanceCompletes c@(Completes q ms mi) =
  case mi of
    Nothing ->
      if Seq.null ms
        then c
        else Completes q ms (Just 0)
    Just i ->
      let i' = i + 1
      in if i' >= Seq.length ms
        then Completes q ms Nothing
        else Completes q ms (Just i')
