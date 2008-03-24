{-
   Kaos - A compiler for creatures scripts
   Copyright (C) 2005-2008  Bryan Donlan

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Kaos.SeqT (SeqT(..), getNext, runSeqT) where

import Control.Monad.State
import Control.Monad.Trans

newtype SeqT e m a = SeqT { unSeqT :: StateT e m a }
    deriving (Monad, MonadTrans, MonadIO, Functor)

getNext :: (Enum e, Monad m) => SeqT e m e
getNext = SeqT $ do
    n <- get
    put $ succ n
    return n

runSeqT :: (Enum e, Monad m) => e -> SeqT e m r -> m r
runSeqT i (SeqT m) = evalStateT m i
