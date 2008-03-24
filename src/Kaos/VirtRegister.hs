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

module Kaos.VirtRegister (VirtRegister(..), VRegAllocT, runVRegAllocT, newVReg) where

import Kaos.SeqT
import Control.Monad.State
import Data.Generics
import Kaos.KaosM

newtype VirtRegister = VR Int
    deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)

newtype VRegAllocT m a = RT (SeqT VirtRegister m a)
    deriving (Monad, MonadTrans, MonadKaos, KaosDiagM)

instance (MonadState s m) => MonadState s (VRegAllocT m) where
    get = lift get
    put = lift . put

runVRegAllocT :: (Monad m) => VRegAllocT m a -> m a
runVRegAllocT (RT m ) = runSeqT (VR 0) m

newVReg :: (Monad m) => VRegAllocT m VirtRegister
newVReg = RT getNext
