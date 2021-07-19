{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Recruit.Orphans where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text.Zipper (TextZipper)
import GHC.Generics (Generic)
import LittleLogger.Manual (Severity)
import SimpleParser (Chunk, CompoundError (..), ErrorExplanation (..), RawError (..), StreamError (..), TextLabel (..),
                     Token)
import TextShow (Builder, FromStringShow (..), TextShow (..), TextShow1 (..), showbPrec1, showbUnaryWith)
import TextShow.Generic (FromGeneric (..))

deriving stock instance Generic ErrorExplanation
deriving via FromGeneric ErrorExplanation instance TextShow ErrorExplanation
deriving stock instance Generic TextLabel
deriving via FromGeneric TextLabel instance TextShow TextLabel
deriving stock instance Generic (RawError c t)
deriving via FromGeneric (RawError c t) instance (TextShow c, TextShow t) => TextShow (RawError c t)
deriving stock instance Generic (StreamError s)
deriving via FromGeneric (StreamError s) instance (TextShow (Chunk s), TextShow (Token s)) => TextShow (StreamError s)
deriving stock instance Generic (CompoundError s e)
deriving via FromGeneric (CompoundError s e) instance (TextShow (StreamError s), TextShow e) => TextShow (CompoundError s e)

deriving via FromStringShow Severity instance TextShow Severity

deriving via FromStringShow (TextZipper a) instance (Show a) => TextShow (TextZipper a)

-- The following are from text-show-instances:
-- https://hackage.haskell.org/package/text-show-instances-3.8.4
-- BSD-licensed, (C) 2014-2017 Ryan Scott

showbUnaryListWith :: ([a] -> Builder) -> Int -> [a] -> Builder
showbUnaryListWith sl = showbUnaryWith (const sl) "fromList"
{-# INLINE showbUnaryListWith #-}

instance TextShow a => TextShow (Seq a) where
  showbPrec = showbPrec1
  {-# INLINE showbPrec #-}

instance TextShow1 Seq where
  liftShowbPrec _ sl p = showbUnaryListWith sl p . toList
  {-# INLINE liftShowbPrec #-}
