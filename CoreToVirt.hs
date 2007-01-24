module CoreToVirt (coreToVirt) where

import Core
import VirtRegister
import Slot
import KaosM
import CAOS

import Data.List

import qualified Data.Map as M

coreToVirt :: Core -> KaosM (CAOS VirtRegister)
coreToVirt = runVRegAllocT . c2v

mapDual f = concatDual . map f

concatDual ::
       [future -> (future, a)]
    -> (future -> (future, [a]))
concatDual [] = (,[])
concatDual (h:t) = \future ->
    let (remFut, remV)    = concatDual t future
        (locFut, locV)    = h remFut
    in  (locFut, locV:remV)

withFuture :: (future -> a) -> (future -> (future, a))
withFuture f = \future -> (future, f a)

liftF f (future, value) = (future, f value)

sequenceF = liftF sequence

mapDualM f = sequenceF . mapDual f

data Lookahead =
    Bound VirtRegister
  | Read
  | Mutate

data State =
    Private VirtRegister
  | Shared  VirtRegister
  | Const   ConstValue

c2v b = snd $ transBlock b M.empty

transBlock b = liftF (liftM concat) $ mapDualM transLine b

transLine (CoreNote _) = liftF (return ())
transLine (CoreLine l) = \future ->
    (updateLookahead future,
     emitLine future
    )
    where
        access = lineAccess l
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

        emitLine future = do
            mapM_ (realloc future) access
            s <- get
            return $ CAOSLine $ map (emitToken s)

        realloc future (slot, access) =
            s <- get
            let curState  = M.lookup slot s
            let lookahead = M.lookup slot future
            s' <- realloc' curState lookahead access
            put $ M.alter (const s') slot s
            where
                -- make sure we go to the bound reg when we need to
                realloc' _ (Just (Bound b)) WriteAccess
                    = return (Just b)
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
                realloc' s@(Just (Private s)) _ MutateAccess
                    = return s
                realloc' s l a@MutateAccess
                    = error $ "ICE: realloc: found a mutate in wrong state: " ++ show (s, l, a)
                realloc' s@(Just _) _ ReadAccess
                    = return s
                realloc' s l a = error $ "ICE: realloc: unhandled case: " ++ show (s,l,a)
        emitToken (TokenLiteral l) s = CAOSLiteral l
        emitToken (TokenConst   c) s = CAOSConst c
        emitToken (TokenRegister r) s = r'
            where
                r' = case M.lookup r s of
                        Nothing -> error $ "ICE: emitToken: reg lookup FAILED: " ++ show (r, s)
                        Just (Private r_) -> CAOSRegister r_
                        Just (Shared  r_) -> CAOSRegister r_
                        Just (ConstValue cv) -> CAOSConst cv

                
transLine (CoreConst s c) = \future ->
    (updateLookahead future, emit future)
    where
        verb = "setv" -- XXX
        zap  = modify $ M.delete s
        -- This is a write operation, so nuke our entry
        updateLookahead future = M.delete r future
        emit future =
            case M.lookup s future of
                Nothing -> do
                    zap -- We're not needed!
                    return ()
                Just (Bound r) -> do
                    modify $ M.insert s (Private r)
                    return [CAOSLiteral verb
                           ,CAOSRegister r
                           ,CAOSConst c
                           ]
                Just Read -> do -- yay const propagation!
                    modify $ M.insert s (Const c)
                    return []
                Just Mutate -> do
                    r <- newReg
                    modify $ M.insert s (Private r)
                    return [CAOSLiteral verb
                           ,CAOSRegister r
                           ,CAOSConst c
                           ]

transLine core@(CoreAssign s1 s2) = \future ->
    (updateLookahead future, emit future)
    where
        verb = "setv" -- XXX
        
        updateLookahead future =
            let s1f  = M.lookup s1 future
                s2f  = M.lookup s2 future
                s1f' = updSrc  s1f s2f
                s2f' = updDest s1f s2f 
            in  M.alter (const s1f') s1 $ M.alter (const s2f') s2 future
        
        -- If the source has no future, we can just rename it over the
        -- destination, so propagate the destination's future
        updSrc Nothing x = x
        -- Otherwise, we can't mess with its future, as it still is needed upstream
        updSrc (Just x) _ = Just x
        -- We're clobbering the destination, so allow it to wander downstream
        updDest _ _ = Nothing

        emit future =
            let s1f  = M.lookup s1 future
                s2f  = M.lookup s2 future
            in  doAssign s1f s2f

        makeShared Nothing     = error $ "transLine CoreAssign: State was Nothing when making shared in " ++ show core
        makeShared (Private r) = Shared r
        makeShared r           = r

        -- Case 1: The source register is not being used later.
        --   Rename the source over the dest and leave it at that.
        doAssign Nothing _ = do
            s <- get
            modify $ M.alter (const $ M.lookup s1 s) s2
            modify $ M.delete s1
            return []
        -- Maybe we're not needed at all?
        doAssign _ Nothing = do
            modify $ M.delete s2
        -- Not as nice, but okay, it's read-only, so we can alias
        doAssign (Just Read) (Just Read) = do
            modify $ M.alter makeShared s1
            s <- get
            modify $ M.alter (const $ M.lookup s1 s) s2
            return []
        -- Otherwise, one or the other's getting overwritten, so fix it now.
        -- In the case of a bound variable, we have the register already.
        doAssign _ (Just (Bound r)) = assignTo r
        doAssign _ _ = do
            r <- newReg
            assignTo r

        -- Perform copying assignment. Slot 2's new register is in r
        assignTo r = do
            modify $ M.alter (const $ Just $ Private r) s2
            s <- get
            s1r <- case (M.lookup s s1) of
                        Just (Private r) -> return $ CAOSRegister r
                        Just (Shared r) -> return $ CAOSRegister r
                        Just (Const c) -> return $ CAOSConst c
                        x -> fail $ "ICE: assignTo: impossible s1 state: " ++ show x
            return [CAOSLiteral verb
                   ,CAOSRegister r
                   ,s1r
                   ]
