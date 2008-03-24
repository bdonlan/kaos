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

module Kaos.PrettyM (PrettyM, runPretty, emitLine, newLine, withIndent, prefixFirst, prefixAll) where

import Control.Monad.Reader
import Control.Monad.Writer

data PState = PS { indent :: Int }

newtype PrettyM a = PrettyM { unlift :: WriterT String (Reader PState) a }
    deriving (Monad)

liftP :: WriterT String (Reader PState) a -> PrettyM a
liftP = PrettyM

runPretty :: PrettyM a -> String
runPretty m = runReader (execWriterT $ unlift m) (PS 0)

emitLine :: String -> PrettyM ()
emitLine l = do
    i <- liftP $ asks indent
    liftP $ tell $ (replicate i ' ') ++ l ++ "\n"

newLine :: PrettyM ()
newLine = emitLine ""

withIndent :: Int -> PrettyM t -> PrettyM t
withIndent i = prefixAll $ i `replicate` ' '

prefixAll :: String -> PrettyM t -> PrettyM t
prefixAll prefix = liftP . censor f . unlift
    where
        f = unlines . map (prefix ++) . lines

prefixFirst :: String -> PrettyM t -> PrettyM t
prefixFirst prefix = liftP . censor f . unlift
    where
        pad = (length prefix) `replicate` ' '
        f string =
            case lines string of
                [] -> [] -- This shouldn't happen... right?
                (h:t) -> unlines $ (prefix ++ h):(map (pad ++) t)
