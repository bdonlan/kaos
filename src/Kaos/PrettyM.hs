{-# OPTIONS_GHC -fglasgow-exts #-}
module Kaos.PrettyM (PrettyM, runPretty, emitLine, newLine, withIndent, prefixFirst, prefixAll) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

data PState = PS { indent :: Int }

newtype PrettyM a = PrettyM { unlift :: WriterT String (Reader PState) a }
    deriving (Monad)

liftP = PrettyM

runPretty m = runReader (execWriterT $ unlift m) (PS 0)

emitLine l = do
    i <- liftP $ asks indent
    liftP $ tell $ (replicate i ' ') ++ l ++ "\n"

newLine = emitLine ""

withIndent i = prefixAll $ i `replicate` ' '

prefixAll prefix = liftP . censor f . unlift
    where
        f = unlines . map (prefix ++) . lines

prefixFirst prefix = liftP . censor f . unlift
    where
        pad = (length prefix) `replicate` ' '
        f string =
            case lines string of
                [] -> [] -- This shouldn't happen... right?
                (h:t) -> unlines $ (prefix ++ h):(map (pad ++) t)
