module CoreToVirt (coreToVirt) where

import AST
import Core
import VirtRegister
import Slot
import KaosM
import CAOS

import Data.List
import Data.Maybe
import Debug.Trace
import Control.Monad.State hiding (State)

import qualified Data.Map as M

coreToVirt :: Core Slot -> KaosM (CAOS VirtRegister)
coreToVirt c = evalStateT (runVRegAllocT $ c2v c) M.empty

mapDual f = concatDual . map f

concatDual ::
       [future -> (future, a)]
    -> (future -> (future, [a]))
concatDual [] = \future -> (future, [])
concatDual (h:t) = \future ->
    let (remFut, remV)    = concatDual t future
        (locFut, locV)    = h remFut
    in  (locFut, locV:remV)

withFuture :: (future -> a) -> (future -> (future, a))
withFuture f = \future -> (future, f future)

liftF ::
      (a -> b)
   -> (future -> (future, a))
   -> (future -> (future, b))
liftF f g future =
    let (future', a) = g future
    in  (future', f a)

returnF v = \future -> (future, v)

sequenceF :: Monad m =>
       (future -> (future, [m a]))
   ->  (future -> (future, m [a]))
sequenceF = liftF sequence

mapDualM :: Monad m =>
     (t -> future -> (future, m a))
  -> [t]
  -> (future -> (future, m [a]))
mapDualM f = sequenceF . mapDual f

data Lookahead =
    Bound VirtRegister
  | Read
  | Mutate
  deriving (Show, Eq)

data State =
    Private VirtRegister
  | Shared  VirtRegister
  | Const   ConstValue
  deriving (Show, Eq)

type SlotStorage = Maybe State
type SlotFuture  = Maybe Lookahead

type TransM = VRegAllocT (StateT (M.Map Slot State) KaosM)
type MarkedLine = CoreLine (SlotStorage, Slot, SlotFuture)
type MarkedBlock = CoreBlock (SlotStorage, Slot, SlotFuture)

c2v b = do
    marked <- snd $ markBlock b M.empty
    transBlock marked

markBlock b = mapDualM markLine b

markLine ::
       CoreLine Slot
    -> M.Map Slot Lookahead
    -> (M.Map Slot Lookahead, TransM (CoreLine (SlotStorage, Slot, SlotFuture)))
markLine n@(CoreNote note) = returnF (return (CoreNote note))
markLine (CoreTouch sa@(SA s a)) = \future ->
    let (future', emit') = markLine (CoreLine [TokenSlot sa]) future
    in  (future', do CoreLine [TokenSlot sa'] <- emit'
                     return (CoreTouch sa')
        )
markLine line@(CoreLine l) = \future ->
    (updateLookahead future,
     doMark future
    )
    where
        access = lineAccess line
        updateLookahead future =
            foldl updateReg future access
        updateReg future (slot, access) =
            M.alter (mergeLA access) slot future
        -- If we overwrite a register, we have an opportunity to relocate
        -- it. So we don't care what happens in the future.
        mergeLA WriteAccess _          = Nothing
        -- Otherwise, we need to preserve fixed virtreg bindings.
        mergeLA _ (Just (Bound r))     = Just $ Bound r
        -- Mutation swallows up read accesses...
        mergeLA _ (Just Mutate)        = Just Mutate
        -- as well as anything else happening up there in the future
        mergeLA MutateAccess _         = Just Mutate
        -- We should only have various read cases here. But just in case,
        -- let's explicitly match.
        mergeLA ReadAccess (Just Read) = Just Read
        mergeLA ReadAccess Nothing     = Just Read
        -- The identity element ... this shouldn't actually appear :)
        mergeLA NoAccess x             = x
        mergeLA x y = error $ "ICE: mergeLA unhandled case: " ++ show (x, y)

        doMark :: M.Map Slot Lookahead -> TransM (CoreLine (SlotStorage, Slot, SlotFuture))
        doMark future = do
            mapM_ (realloc future) access
            s <- get
            let line = CoreLine $ map (markToken s future) l
            return line

        realloc future (slot, access) = do
            s <- get
            let curState  = M.lookup slot s
            let lookahead = M.lookup slot future
            s' <- realloc' curState lookahead access
            modify $ M.alter (const s') slot
            where
                realloc' ::
                       (Maybe State)
                    -> (Maybe Lookahead)
                    -> (AccessType)
                    -> TransM (Maybe State)
                -- make sure we go to the bound reg when we need to
                realloc' _ (Just (Bound b)) WriteAccess
                    = return (Just $ Private b)
                -- and now make sure we went to the bound reg in the past :)
                realloc' s l@(Just (Bound b)) a
                    | ((a == MutateAccess) && s /= Just (Private b)) || isNothing s
                    = error $ "ICE: realloc: found a bound reg not in proper state: " ++ show (s, l, a)
                    | otherwise
                    = return s
                realloc' s _ NoAccess = return s
                realloc' _ _ WriteAccess = do
                    r <- newVReg
                    return $ Just (Private r)
                realloc' s@(Just (Private _)) _ MutateAccess
                    = return s
                realloc' s l a@MutateAccess
                    = error $ "ICE: realloc: found a mutate in wrong state: " ++ show (s, l, a)
                realloc' s@(Just _) _ ReadAccess
                    = return s
                realloc' s l a = error $ "ICE: realloc: unhandled case: " ++ show (s,l,a)
        markToken :: M.Map Slot State -> M.Map Slot Lookahead -> CoreToken Slot -> CoreToken (SlotStorage, Slot, SlotFuture)
        markToken s future (TokenLiteral l) = TokenLiteral l
        markToken s future (TokenConst   c) = TokenConst c
        markToken s future (TokenSlot (SA r a)) = TokenSlot (SA (M.lookup r s, r, M.lookup r future) a)

                
markLine line@(CoreConst s c) = \future ->
    (updateLookahead future, emit future)
    where
        verb = "setv" -- XXX
        zap  = modify $ M.delete s
        -- This is a write operation, so nuke our entry
        updateLookahead future = M.delete s future
        emit :: M.Map Slot Lookahead
             -> TransM MarkedLine
        emit future =
            case M.lookup s future of
                Nothing -> do
                    zap -- We're not needed!
                    markMe future
                Just (Bound r) -> do
                    modify $ M.insert s (Private r)
                    markMe future
                Just Read -> do -- yay const propagation!
                    modify $ M.insert s (Const c)
                    markMe future
                Just Mutate -> do
                    r <- newVReg
                    modify $ M.insert s (Private r)
                    markMe future
        markMe future = do
            st <- get
            return $ CoreConst (M.lookup s st, s, M.lookup s future) c

markLine core@(CoreAssign dest src) = \future ->
    (updateLookahead future, emit future)
    where
        verb = "setv" -- XXX
        
        updateLookahead future =
            let srcf  = M.lookup src future
                destf  = M.lookup dest future
                srcf' = updSrc  srcf destf
                destf' = updDest srcf destf 
            in  M.alter (const srcf') src $ M.alter (const destf') dest future
        
        updSrc :: Maybe Lookahead
               -> Maybe Lookahead
               -> Maybe Lookahead
        -- If the source has no future, we can just rename it over the
        -- destination, so propagate the destination's future
        updSrc Nothing x = x
        -- Otherwise, we can't mess with its future, as it still is needed upstream
        updSrc (Just x) _ = Just x

        updDest :: Maybe Lookahead
                -> Maybe Lookahead
                -> Maybe Lookahead
        -- We're clobbering the destination, so allow it to wander downstream
        updDest _ _ = Nothing

        emit :: M.Map Slot Lookahead
             -> TransM MarkedLine
        emit future =
            let srcf  = M.lookup src future
                destf  = M.lookup dest future
            in  do
                    doAssign srcf destf
                    s <- get
                    return $ CoreAssign (M.lookup dest s, dest, destf)
                                        (M.lookup src  s,  src,  srcf)

        makeShared Nothing     = error $ "transLine CoreAssign: State was Nothing when making shared in " ++ show core
        makeShared (Just (Private r)) = Just $ Shared r
        makeShared r           = r

        doAssign :: Maybe Lookahead
                 -> Maybe Lookahead
                 -> TransM ()
        -- Case 1: The source register is not being used later.
        --   Rename the source over the dest and leave it at that.
        doAssign Nothing _ = do
            s <- get
            modify $ M.alter (const $ M.lookup src s) dest
            modify $ M.delete src
        -- Maybe we're not needed at all?
        doAssign _ Nothing = do
            modify $ M.delete dest
        -- Not as nice, but okay, it's read-only, so we can alias
        doAssign (Just Read) (Just Read) = do
            modify $ M.alter makeShared src
            s <- get
            modify $ M.alter (const $ M.lookup src s) dest
        -- Otherwise, one or the other's getting overwritten, so fix it now.
        -- In the case of a bound variable, we have the register already.
        doAssign _ (Just (Bound r)) = assignTo r
        doAssign _ _ = do
            r <- newVReg
            assignTo r
        assignTo r = do
            modify $ M.alter (const $ Just $ Private r) dest
markLine l = fail $ "Unknown linepattern: " ++ show l


transBlock :: MarkedBlock -> TransM (CAOSBlock VirtRegister)
transBlock = liftM concat . mapM transLine

transLine :: MarkedLine -> TransM [CAOSLine VirtRegister]
transLine (CoreNote n) = return []
transLine (CoreTouch _) = return []
transLine line@(CoreLine l) = do
        tokens <- mapM transToken l
        return [CAOSLine tokens]
    where
        transToken (TokenLiteral l) = return $ CAOSLiteral l
        transToken (TokenConst c) = return $ CAOSConst c
        transToken (TokenSlot (SA (storage, _, _) _)) = transStorage storage
        transStorage (Just (Private r)) = return $ CAOSRegister r
        transStorage (Just (Shared r)) = return $ CAOSRegister r
        transStorage (Just (Const c)) = return $ CAOSConst c
        transStorage x = fail $ "transLine: register storage in bad state: " ++ show line
transLine (CoreConst s@(storage, _, _) c) = checkAssign storage
    where
        checkAssign (Just (Private r)) = doAssign r
        checkAssign (Just (Const _))   = return []
        checkAssign Nothing            = return []
        checkAssign x                  = fail $ "doConstAssign: unexpected storage " ++ show x
        doAssign r = doAssignType (constType c) (CAOSRegister r) (CAOSConst c)
transLine l@(CoreAssign (destStorage, dest, destFuture) (srcStorage, src, srcFuture)) =
    checkAssign destStorage destFuture srcStorage srcFuture
    where
        checkAssign _ _ _ Nothing = return [] -- rename
        checkAssign (Just (Shared _)) _ _ _ = return [] -- alias
        checkAssign _ Nothing _ _ = return [] -- unused
        checkAssign (Just (Private r)) _ (Just srcStorage) _ = doAssign r srcStorage

        checkAssign _ _ _ _ = fail $ "checkAssign: impossible state " ++ show l
        doAssign r (Private s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSRegister s)
        doAssign r (Shared s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSRegister s)
        doAssign r (Const s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSConst s)

doAssignType :: CAOSType -> CAOSToken a -> CAOSToken a -> TransM (CAOS a)
doAssignType t dest src
    | length clauses == 0
    = fail "Void type in assignment"
    | length clauses == 1
    = let [(_, body)] = clauses
      in  return [body]
    | otherwise
    = do let (hcond, hbody):t = clauses
         return $ [ CAOSLine ((CAOSLiteral "doif"):hcond), hbody] ++ fmt t ++
                  [ CAOSLine [CAOSLiteral "endi"] ]
    where
        fmt [] = []
        fmt ((hcond, hbody):t) =
            [ CAOSLine ((CAOSLiteral "elif"):hcond), hbody ] ++ fmt t
        allverbs =
            [(typeNum, ([CAOSLiteral "le", CAOSConst $ CInteger 1], "setv"))
            ,(typeStr, ([CAOSLiteral "eq", CAOSConst $ CInteger 2], "sets"))
            ,(typeObj, ([CAOSLiteral "eq", CAOSConst $ CInteger 3], "seta"))]
        possverbs = filter ((/= typeVoid) . (typeAnd t) . fst) allverbs
        clauses = map makeclause possverbs
        makeclause (_, (cond, verb)) =
            (cond, CAOSLine [CAOSLiteral verb, dest, src])


