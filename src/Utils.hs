module Utils (join, (??)) where

cond ?? (ifTrue, ifFalse)
    | cond
    = ifTrue
    | otherwise
    = ifFalse

join jl [] = []
join jl [x] = x
join jl (head:tail) = head ++ jl ++ join jl tail
