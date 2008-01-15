module Kaos.AliasMap (AliasMap, empty, aliases, aliasTo, remove, getGen, AMGen, knownItems, nextGen) where

import qualified Data.Map as M

import Data.Generics

newtype AMGen = AMG Int deriving (Eq, Ord, Show)

data AliasMap t = AM    { items  :: M.Map t Int
                        , nextId :: Int
                        }
                        deriving (Data, Typeable, Eq, Ord, Show)

empty :: Ord t => AliasMap t
empty = AM M.empty 0

getId :: Ord t => t -> AliasMap t -> Maybe Int
getId item am = M.lookup item (items am)

vivifyItem :: Ord t => t -> AliasMap t -> (AliasMap t, Int)
vivifyItem item am =
    case getId item am of
        Just i -> (am, i)
        Nothing -> (am { items = M.insert item (nextId am) (items am)
                       , nextId = succ $ nextId am
                       }, nextId am)

aliases :: Ord t => t -> t -> AliasMap t -> Bool
aliases i1 i2 am =
    case (getId i1 am, getId i2 am) of
        (Just n1, Just n2) -> (n1 == n2)
        _ -> False

aliasTo :: Ord t => t -> t -> AliasMap t -> AliasMap t
aliasTo dest item am =
    let (am', destid) = vivifyItem dest am
    in  am' { items = M.insert item destid (items am) }

-- Force the gen counter to increase
remove :: Ord t => t -> AliasMap t -> AliasMap t
remove item am = fst . vivifyItem item $ am { items = M.delete item (items am) }

getGen :: Ord t => t -> AliasMap t -> Maybe AMGen
getGen item = fmap AMG . getId item

knownItems :: Ord t => AliasMap t -> [t]
knownItems = M.keys . items

nextGen :: Ord t => AliasMap t -> AMGen
nextGen = AMG . nextId
