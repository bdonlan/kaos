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
module Kaos.Dump (dumpMap) where

import qualified Data.Map as M

dumpMap :: (Show k, Show v, Ord k) => M.Map k v -> String
dumpMap = unlines . map pairToLine . M.toList
    where pairToLine (k, v) = (show k) ++ " => " ++ (show v)
