module Kaos.AliasMap (
-- types
    AliasMap, empty, GroupMap,
-- aliasing information
    aliases, aliasTo, remove, mergeForUnchanged,
-- grouping
    setGroupVal, getGroupVal, mergeGroup,
-- dark magic
    getGen, AMGen, knownItems, nextGen) where

import Control.Monad.State
import qualified Data.Map as M

import Data.Generics
import Data.Maybe

newtype AMGen = AMG Int deriving (Eq, Ord, Show)

data GroupEntry v = Direct (Maybe v)
                  | Indirect Int
                  deriving (Data, Typeable, Eq, Ord, Show)

data GroupMap k v = AM  { items  :: M.Map k Int
                        , nextId :: Int
                        , groups :: M.Map Int (Int, GroupEntry v)
                        }
                        deriving (Data, Typeable, Eq, Ord, Show)

type AliasMap t = GroupMap t ()

type GMM k v t = State (GroupMap k v) t

--wrapRet :: (GMM k v t) -> (GroupMap k v -> (t, GroupMap k v))
--wrapRet = runState

wrapVoid :: (GMM k v ()) -> (GroupMap k v -> GroupMap k v)
wrapVoid = execState

wrapPure :: (GMM k v r) -> (GroupMap k v -> r)
wrapPure = evalState

empty :: Ord t => GroupMap t v
empty = AM M.empty 0 M.empty

newGroup :: Ord k => Maybe v -> GMM k v Int
newGroup v = do
    st <- get
    let gid = nextId st
    put $ st { groups = M.insert gid (1, Direct v) (groups st)
             , nextId = succ gid
             }
    return gid

unrefGroup :: Ord k => Int -> GMM k v ()
unrefGroup gid = do
    st <- get
    let v = M.lookup gid (groups st)
    v' <- decrc v
    modify $ \st' -> st' { groups = M.alter (const v') gid (groups st') }
    where
        decrc (Just (1, Indirect n)) = do
            unrefGroup n
            return Nothing
        decrc (Just (1, Direct _)) = return Nothing
        decrc (Just (n, v)) = return $ Just (pred n, v)
        decrc _ = error "unreferencing dead group"

refGroup :: Ord k => Int -> GMM k v ()
refGroup gid = do
    modify $ \st -> st { groups = M.alter incrc gid (groups st) }
    where
        incrc Nothing = Just (1, Direct Nothing)
        incrc (Just (rc, v)) = Just (succ rc, v)

getBaseGroup :: Ord k => Int -> GMM k v Int
getBaseGroup gid = do
    st <- get
    case M.lookup gid (groups st) of
        Nothing -> error "missing group"
        Just (_, Indirect gid') -> getBaseGroup gid'
        _ -> return gid

getGroupVal :: Ord k => k -> GroupMap k v -> Maybe v
getGroupVal item = wrapPure $ getGroupVal' item

getGroupVal' :: Ord k => k -> GMM k v (Maybe v)
getGroupVal' item = do
    gid  <- vivifyItem item
    bgid <- getBaseGroup gid
    when (gid /= bgid) $ addToGid bgid item
    st <- get
    case M.lookup bgid (groups st) of
        Nothing -> undefined -- we shouldn't even get this far
        Just (_, Indirect _) -> undefined -- likewise
        Just (_, Direct v) -> return v

setGroupVal :: Ord k => k -> Maybe v -> GroupMap k v -> (GroupMap k v)
setGroupVal item val = wrapVoid $ setGroupVal' item val

setGroupVal' :: Ord k => k -> Maybe v -> GMM k v ()
setGroupVal' item value = do
    gid  <- vivifyItem item
    bgid <- getBaseGroup gid
    when (gid /= bgid) $ addToGid bgid item
    ngid <- newGroup value
    modify $ \st -> st { groups = M.adjust (\(rc, Direct _) -> (rc, Indirect ngid)) bgid (groups st) }

addToGid :: Ord k => Int -> k -> GMM k v ()
addToGid gid item = do
    refGroup gid
    st <- get
    let old = M.lookup item (items st)
    put $ st { items = M.insert item gid (items st) }
    when (isJust old) $ unrefGroup (fromJust old)

vivifyItem :: Ord k => k -> GMM k v Int
vivifyItem item = do
    st <- get
    case M.lookup item (items st) of
        Just gid -> return gid
        Nothing  -> do
            gid <- newGroup Nothing
            modify $ \st' -> st' { items  = M.insert item gid (items st') }
            return gid

aliases :: Ord k => k -> k -> GroupMap k v -> Bool
aliases i1 i2 = wrapPure $ aliases' i1 i2

aliases' :: Ord k => k -> k -> GMM k v Bool
aliases' i1 i2 = do
    g1 <- getBaseGroup =<< vivifyItem i1
    g2 <- getBaseGroup =<< vivifyItem i2
    return (g1 == g2)

aliasTo :: Ord k => k -> k -> GroupMap k v -> GroupMap k v
aliasTo g i = wrapVoid $ aliasTo' g i

aliasTo' :: Ord k => k -> k -> GMM k v ()
aliasTo' group item = do
    gid <- getBaseGroup =<< vivifyItem group
    addToGid gid item

remove :: Ord k => k -> GroupMap k v -> GroupMap k v
remove i = wrapVoid $ remove' i

remove' :: Ord k => k -> GMM k v ()
remove' item = do
    st <- get
    case M.lookup item (items st) of
        Just gid -> unrefGroup gid
        Nothing  -> return ()
    modify $ \st' -> st' { items = M.delete item (items st') }
    vivifyItem item
    return ()

mergeGroup :: Ord k => (Maybe v -> Maybe v -> Maybe v)
                    -> k
                    -> k
                    -> GroupMap k v
                    -> GroupMap k v
mergeGroup f a b = wrapVoid $ mergeGroup' f a b

mergeGroup' :: Ord k => (Maybe v -> Maybe v -> Maybe v)
                     -> k
                     -> k
                     -> GMM k v ()
mergeGroup' mergeFunc g1 g2 = do
    gid1 <- getBaseGroup =<< vivifyItem g1
    gid2 <- getBaseGroup =<< vivifyItem g2

    when (gid1 /= gid2) $ do
        val1 <- getGroupVal' g1
        val2 <- getGroupVal' g2
        let newVal = mergeFunc val1 val2
        refGroup gid1
        modify $ \st' -> st' { groups = M.adjust (\(rc, _) -> (rc, Indirect gid1)) gid2 (groups st') }
        setGroupVal' g1 newVal

getGen :: Ord k => k -> GroupMap k v -> AMGen
getGen i = AMG . (wrapPure (getBaseGroup =<< vivifyItem i))
knownItems :: Ord k => GroupMap k v -> [k]
knownItems = M.keys . items 
nextGen :: Ord k => GroupMap k v -> AMGen
nextGen = AMG . nextId
mergeForUnchanged :: Ord k  => GroupMap k v
                            -> GroupMap k v
                            -> GroupMap k v
                            -> GroupMap k v
mergeForUnchanged origin t1 t2
    = foldl checkKey t1 (knownItems t1)
    where
        checkKey t item
            | not keep
            = remove item t
            | otherwise
            = t
            where
            keep = (getGen item t1) == (getGen item t2)
                && (getGen item t1) == (getGen item origin)
