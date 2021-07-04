{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Buh.Orphans where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text.Zipper (TextZipper)
import LittleLogger.Manual (Severity)
import TextShow (Builder, FromStringShow (..), TextShow (..), TextShow1 (..), showbPrec1, showbUnaryWith)

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
