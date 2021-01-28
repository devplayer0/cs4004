{-# LANGUAGE TemplateHaskell, MultiWayIf #-}
module CSP where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.PQueue.Min as Q

import Control.Monad.State
import Control.Monad.Trans.Maybe

import Control.Lens

import Debug.Trace

insertAll' :: Ord k => [k] -> a -> Map.Map k a -> Map.Map k a
insertAll' [] _ m = m
insertAll' (key:keys) v m = insertAll' keys v (Map.insert key v m)

insertAll :: Ord k => Set.Set k -> a -> Map.Map k a -> Map.Map k a
insertAll = insertAll' . Set.toList

keys :: Getter (Map.Map k a) (Set.Set k)
keys = to Map.keysSet

type Pos = (Int, Int)
type Bounds = Pos

_x :: Field1 s t a b => Lens s t a b
_x = _1
_y :: Field2 s t a b => Lens s t a b
_y = _2

data Square = Square {
  _squarePos :: Pos
, _mines :: Int
}
makeLenses ''Square

data Move = Move {
  _movePos :: Pos
, _mineProbability :: Float
} deriving (Show)
makeLenses ''Move

instance Eq Move where
  x == y = (x^.movePos) == (y^.movePos)
instance Ord Move where
  x `compare` y = (x^.mineProbability) `compare` (y^.mineProbability)

data Constraint = Constraint {
  _lhs :: Set.Set Pos
, _rhs :: Int
} deriving (Show)
makeLenses ''Constraint

instance Eq Constraint where
  x == y = (x^.lhs) == (y^.lhs)

constraintTuple :: (Set.Set Pos, Int) -> Constraint
constraintTuple (lhs, rhs) = Constraint lhs rhs

isEmpty :: Getter Constraint Bool
isEmpty = to (\c -> Set.null (c^.lhs))

type Constraints = Map.Map (Set.Set Pos) Int
constraintsList :: Constraints -> [Constraint]
constraintsList = (map constraintTuple) . Map.toList

insertConstraint :: Constraint -> Constraints -> Constraints
-- only do the insert if the constraint isn't empty!
insertConstraint c cs
  | c^.isEmpty = cs
  | otherwise = Map.insert (c^.lhs) (c^.rhs) cs

data Minesweeper = Minesweeper {
  _boardSize :: Bounds
, _mineCount :: Int
, _variables :: Map.Map Pos Bool
, _constraints :: Constraints
, _moveCache :: Q.MinQueue Move
} deriving (Show)

makeLenses ''Minesweeper

newMinesweeper s n = Minesweeper {
  _boardSize = s, _mineCount = n,
  _variables = Map.empty, _constraints = Map.empty, _moveCache = Q.empty
}

type MinesweeperState a = State Minesweeper a

neighbours :: Pos -> Bounds -> Set.Set Pos
neighbours p b = execState (do
  let r = modify . flip Set.difference . Set.fromList

  when (x == 0)         (r $ [(x-1,y-1),(x-1,y),(x-1,y+1)])
  when (y == 0)         (r $ [(x-1,y-1),(x,y-1),(x+1,y-1)])
  when (x == b^._x - 1) (r $ [(x+1,y-1),(x+1,y),(x+1,y+1)])
  when (y == b^._y - 1) (r $ [(x-1,y+1),(x,y+1),(x+1,y+1)])) ns

  where
    x = p^._x
    y = p^._y
    ns = Set.fromList [(x-1,y-1),(x,y-1),(x+1,y-1), (x-1,y),(x+1,y), (x-1,y+1),(x,y+1),(x+1,y+1)]


removeSubset subset = execState $ do
  lhs %= flip Set.difference (subset^.lhs)
  rhs -= subset^.rhs

removeSubsets' :: [Constraint] -> Constraint -> Constraint
removeSubsets' [] c = c
removeSubsets' (c2:cs) c = removeSubsets' cs $ if
  | (c2^.lhs) `Set.isSubsetOf` (c^.lhs) -> removeSubset c2 c
  | otherwise -> c

-- simplify a constraint by removing existing constraints from it (that are a subset of this one)
removeSubsets :: Constraints -> Constraint -> Constraint
removeSubsets = removeSubsets' . constraintsList

-- simplify a constraint by removing any:
--  - variables whose values are known
--  - existing constraints whose LHS is a subset of the provided constraints LHS
simplify :: Minesweeper -> Constraint -> Constraint
simplify m = execState $ do
  let varKeys = m^.variables.keys

  -- remove any variables we already know
  alreadyKnown <- do
    cVars <- use lhs
    return $ Set.intersection cVars varKeys
  lhs %= flip Set.difference varKeys

  -- update RHS according to the values of variables removed from LHS
  let reducer = (\v total -> total + if (m^.variables) Map.! v then 1 else 0)
  rhs -= (Set.foldr reducer 0 alreadyKnown)

  -- remove subsets
  c <- get
  if
     -- empty constraint - none of the constraints could be a subset of that!
     | c^.isEmpty -> return ()
     | otherwise -> do
       old <- get
       modify $ removeSubsets (m^.constraints)

       new <- get
       if new /= old
        -- constraint has changed - repeat simplification (might be trivial now)
        then modify $ simplify m
        else return ()

addConstraint :: Constraint -> MinesweeperState ()
addConstraint c = do
  s <- get
  let simp = simplify s c

  if
     -- simplification resulted in an empty constraint; do nothing
     | simp^.isEmpty -> return ()
     -- constant is 0, therefore all variables must be 0
     | simp^.rhs == 0 -> do
        variables %= (insertAll (simp^.lhs) False)
        -- also add all of new the variables as playable moves!
        let moves = map (\p -> Move p 0) $ Set.toList (simp^.lhs)
        moveCache %= Q.union (Q.fromAscList moves)
     -- constant matches the number of variables on the LHS, therefore all variables must be 1
     | simp^.rhs == Set.size (simp^.lhs) -> variables %= (insertAll (simp^.lhs) True)
     | otherwise -> do
        cs <- use constraints
        mapM_ (\c2 -> if
          | (simp^.lhs) `Set.isSubsetOf` (c2^.lhs) -> do
            -- remove the existing constraint
            constraints %= Map.delete (c2^.lhs)
            -- re-add the modified one (might incur more simplification etc!)
            addConstraint $ removeSubset simp c2
          | otherwise -> return ()) (constraintsList cs)

        -- finally add the new constraint
        constraints %= (insertConstraint simp)

playMove :: Square -> MinesweeperState ()
playMove (Square pos mines) = do
  -- this counts as a new variable
  variables %= Map.insert pos False

  -- create and add a new constraint based on the neighbouring squares and mines
  bounds <- use boardSize
  let ns = neighbours pos bounds
  --(trace $ show ns) return ()
  addConstraint $ Constraint ns mines

peekNextMove :: Minesweeper -> Maybe Move
peekNextMove m = Q.getMin (m^.moveCache)

nextMove :: MinesweeperState (Maybe Move)
--nextMove = do
--  q <- use moveCache
--  case Q.minView q of
--    Just (move, newCache) -> do
--      moveCache .= newCache
--      return $ Just move
--    Nothing -> return Nothing
nextMove = runMaybeT $ do
  q <- use moveCache
  (move, newCache) <- MaybeT $ return (Q.minView q)
  moveCache .= newCache

  return move

evalMinesweeper :: MinesweeperState a -> Minesweeper -> a
evalMinesweeper = evalState

execMinesweeper :: MinesweeperState a -> Minesweeper -> Minesweeper
execMinesweeper = execState
