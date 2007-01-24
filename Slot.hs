module Slot
( Slot(..), SlotStateT(..), getSlotID,
  Mutation(..), mmerge, mread, mchange, moverwrite,
  SlotMutation(..), newSlot, linkSlotTypes
)
where

import LiftST
import Data.STRef
import AST
import Control.Monad.State
import Register
import Debug.Trace

data Slot s = Slot {
    slotID :: Int,
    slotTypeR :: STRef s ValueType,
    getSlotType :: MonadST s m => m ValueType,
    constrainSlotType :: MonadST s m => ValueType -> m ValueType,
    setReg :: MonadST s m => Register s -> m (),
    getReg :: MonadST s m => m (Maybe (Register s))
    -- TODO
    }

instance Show (Slot s) where
    show s = "#<slot:" ++ (show $ slotID s) ++ ">"

getSlotID = slotID

instance Ord (Slot s) where
    s `compare` s' = (slotID s) `compare` (slotID s')
    
instance Eq (Slot s) where
    a == b = (a `compare` b) == EQ

class SlotStateT m where
    newSlotID :: m Int

newSlot = do
    id <- newSlotID
    tvar <- liftST $ newSTRef vall
    rvar <- liftST $ newSTRef Nothing
    return $ Slot {
        slotID = id,
        slotTypeR = tvar,
        getSlotType = liftST $ readSTRef tvar,
        constrainSlotType = (\t2 -> do
            old <- liftST $ readSTRef tvar
            let new = old `vand` t2
            liftST $ writeSTRef tvar new
            if new == vnone
                then error "unsatisfiable type constraints"
                else return new
                ),
        getReg = liftST $ readSTRef rvar,
        setReg = \r -> liftST $ writeSTRef rvar (Just r)
        }

linkSlotTypes s1 s2 = do
    t1 <- getSlotType s1
    t2 <- getSlotType s2
    let new = t1 `vand` t2
    constrainSlotType s1 new
    constrainSlotType s2 new
    return new

newtype Mutation = Mutation (Bool, Bool)
    deriving (Eq, Ord)

(Mutation (read, write)) `mmerge` (Mutation (read', write')) =
    Mutation (read || read', write || write')

mread = Mutation (True, False)
mchange = Mutation (True, True)
moverwrite = Mutation (False, True)

instance Show Mutation where
    show (Mutation (False, False)) = "#<mut:void>"
    show (Mutation (True, False))  = "#<mut:read>"
    show (Mutation (False, True))  = "#<mut:overwrite>"
    show (Mutation (True, True))   = "#<mut:change>"

data SlotMutation s = SM (Slot s) Mutation
    deriving (Eq)

instance Show (SlotMutation s) where
    show (SM s m) = "#<mutate:" ++ (show s) ++ (show m) ++ ">"

instance Ord (SlotMutation s) where
    (SM s m) `compare` (SM s' m')
        | sc == EQ
        = m `compare` m'
        | otherwise
        = sc
        where
            sc = s `compare` s'
