{-# LANGUAGE TemplateHaskell, MultiWayIf, TupleSections #-}
module CSP where

import Control.Monad
import Control.Arrow
import Control.Monad.Trans.State
import Control.Lens hiding (both)

import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.PQueue.Min as Q

import Debug.Trace

btoi :: Num p => Bool -> p
btoi True  = 1
btoi False = 0

itob :: (Eq a, Num a) => a -> Bool
itob 0  = False
itob _  = True

firstJust :: [Maybe a] -> Maybe a
firstJust ms =
  case filter isJust ms of
    [] -> Nothing
    js -> head js

fmapTuple :: Functor f => (a -> b) -> f a -> f (a, b)
fmapTuple f = fmap (\x -> (x, f x))

sumWith :: (Foldable t, Num b) => (a -> b) -> t a -> b
sumWith f = foldr ((+) . f) 0

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b m = do
  t <- b
  unless t m

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

data MoveType
  = KnownVariable
  | CertainSafe
  | CrapShoot
  | Guess
  | Heuristic
  deriving (Show)

data Move = Move {
  _moveType :: MoveType
, _movePos :: Pos
, _mineProbability :: Float
} deriving (Show)
makeLenses ''Move

instance Eq Move where
  x == y = (x^.movePos) == (y^.movePos)
instance Ord Move where
  x `compare` y = (x^.mineProbability) `compare` (y^.mineProbability)

type Variable = (Pos, Bool)
type Variables = Map.Map Pos Bool

data Constraint = Constraint {
  _lhs :: Set.Set Pos
, _rhs :: Int
} deriving (Show)
makeLenses ''Constraint

instance Eq Constraint where
  x == y = (x^.lhs) == (y^.lhs)

isEmpty_ :: Constraint -> Bool
isEmpty_ c = Set.null $ c^.lhs

isEmpty :: Getter Constraint Bool
isEmpty = to isEmpty_

inConstraint :: Pos -> Constraint -> Bool
inConstraint p = Set.member p . _lhs

isSubConstraint :: Constraint -> Constraint -> Bool
isSubConstraint c c2 = (c^.lhs) `Set.isSubsetOf` (c2^.lhs)

type Constraints = [Constraint]

insertConstraint :: Constraint -> Constraints -> Constraints
-- only do the insert if the constraint isn't empty!
insertConstraint c cs
  | c^.isEmpty = cs
  | otherwise = c:cs

data CSPState = CSPState {
  _boardSize :: Bounds
, _mineCount :: Int
, _variables :: Variables
, _constraints :: [Constraint]
, _played :: Set.Set Pos
--, _moveCache :: Q.MinQueue Move
} deriving (Show)

remainingMines :: CSPState -> Int
remainingMines m = nms - sumWith btoi (Map.elems vars)
  where
    nms = _mineCount m
    vars = _variables m

makeLenses ''CSPState

newMinesweeper s n = CSPState {
  _boardSize = s, _mineCount = n,
  _variables = Map.empty, _constraints = [], _played = Set.empty--, _moveCache = Q.empty
}

type CSP = State CSPState

neighbours :: Bounds -> Pos -> Set.Set Pos
neighbours b p = execState (do
  let r = modify . flip Set.difference . Set.fromList

  when (x == 0)         (r [(x-1,y-1),(x-1,y),(x-1,y+1)])
  when (y == 0)         (r [(x-1,y-1),(x,y-1),(x+1,y-1)])
  when (x == b^._x - 1) (r [(x+1,y-1),(x+1,y),(x+1,y+1)])
  when (y == b^._y - 1) (r [(x-1,y+1),(x,y+1),(x+1,y+1)])) ns

  where
    x = p^._x
    y = p^._y
    ns = Set.fromList [(x-1,y-1),(x,y-1),(x+1,y-1), (x-1,y),(x+1,y), (x-1,y+1),(x,y+1),(x+1,y+1)]

-- Apply a single variable to a constraint (remove it from the LHS and updating the RHS accordingly)
-- Not checked!
applyVariable' :: Variable -> Constraint -> Constraint
applyVariable' (p, v) (Constraint lhs rhs) = Constraint (Set.delete p lhs) (rhs - btoi v)

-- Apply a single variable to a constraint (remove it from the LHS and updating the RHS accordingly)
applyVariable :: Variable -> Constraint -> Constraint
applyVariable var c
  | p `Set.member` lhs = applyVariable' var c
  | otherwise = c
  where
    (Constraint lhs rhs) = c
    (p, _) = var

-- Apply known variables to a constraint (removing them from the LHS and updating
-- the RHS accordingly)
applyVariables :: Variables -> Constraint -> Constraint
applyVariables vs (Constraint lhs rhs) = Constraint lhs2 rhs2
  where
    known = Map.keysSet vs `Set.intersection` lhs

    lhs2 = lhs `Set.difference` known

    reducer = (+) . btoi . (vs Map.!)
    rhs2 = rhs - Set.foldr reducer 0 known

removeSubset :: Constraint -> Constraint -> Constraint
removeSubset (Constraint lhs rhs) (Constraint lhs2 rhs2) =
  Constraint (Set.difference lhs2 lhs) (rhs2 - rhs)

-- simplify a constraint by removing existing constraints from it (that are a subset of this one)
removeConstraintSubsets :: Constraints -> Constraint -> Constraint
removeConstraintSubsets [] c = c
removeConstraintSubsets (c2:cs) c = removeConstraintSubsets cs subsetLess
  where
    subsetLess
      | c2 `isSubConstraint` c = removeSubset c2 c
      | otherwise = c

-- simplify a constraint by removing any:
--  - variables whose values are known
--  - existing constraints whose LHS is a subset of the provided constraints LHS
simplify :: CSPState -> Constraint -> Constraint
simplify m = execState $ do
  -- remove any variables we already know
  modify $ applyVariables (m^.variables)

  -- remove subsets (recursing for potentially further simplification)
  c <- get
  unless (c^.isEmpty) $ do
    old <- get
    modify $ removeConstraintSubsets (m^.constraints)

    new <- get
    when (new /= old) $ modify $ simplify m

-- Update constraints matching a filter (calls `addConstraint` on changed constraints)
updateConstraintsWhere :: (Constraint -> Bool) -> (Constraint -> Constraint) -> CSP ()
updateConstraintsWhere filter' f = do
  cs <- use constraints
  let (changed, unchanged) = List.partition filter' cs

  -- Keep only the constraints that haven't changed
  constraints .= unchanged
  -- Queue an insertion of the modified constraints
  mapM_ (addConstraint . f) changed

addConstraint :: Constraint -> CSP ()
addConstraint c = do
  s <- get
  let simp = simplify s c

  if
     -- simplification resulted in an empty constraint; do nothing
    | simp^.isEmpty -> return ()
    -- constant is 0, therefore all variables must be 0
    | simp^.rhs == 0 -> variables %= insertAll (simp^.lhs) False
    -- constant matches the number of variables on the LHS, therefore all variables must be 1
    | simp^.rhs == Set.size (simp^.lhs) -> variables %= insertAll (simp^.lhs) True
    | otherwise -> do
      -- remove all who had `simp` as a subset
      updateConstraintsWhere (isSubConstraint simp) (removeSubset simp)

      -- finally add the new constraint
      constraints %= insertConstraint simp

-- Play a move (known to be safe)
playMove' :: Square -> CSP ()
playMove' (Square pos mines) = do
  played %= Set.insert pos

  -- This counts as a new variable
  variables.at pos ?= False

  -- Update the constraints with this position in them
  updateConstraintsWhere (inConstraint pos) (applyVariable (pos, False))

  -- Create and add a new constraint based on the neighbouring squares and mines
  bounds <- use boardSize
  let ns = neighbours bounds pos

  addConstraint $ Constraint ns mines

playMove :: Square -> CSPState -> CSPState
playMove = execMinesweeper . playMove'

-- Add a known mine (should be called with the return list of flags from `moves`!)
addMine' :: Pos -> CSP ()
addMine' pos = do
  variables.at pos ?= True

  -- Update the constraints with this position in them
  updateConstraintsWhere (inConstraint pos) (applyVariable (pos, True))

addMine :: Pos -> CSPState -> CSPState
addMine = execMinesweeper . addMine'

-- A constraint is coupled with another if it contains at least one common
-- variable
coupledWith :: Constraint -> Constraints -> (Constraints, Constraints)
coupledWith c = List.partition $ \c2 -> not $ (c^.lhs) `Set.disjoint` (c2^.lhs)

coupledSubset' :: [Constraint] -> [Constraint] -> Constraints -> (Constraints, Constraints)
coupledSubset' checked [] pool = (checked, pool)
coupledSubset' checked (c:cs) pool = coupledSubset' (c:checked) (cs ++ coupled) notCoupled
  where (coupled, notCoupled) = coupledWith c pool

-- A "coupled subset" is a subset of the set of constraints with a chain of at
-- least one common variable
coupledSubset :: Constraints -> (Constraints, Constraints)
coupledSubset (c:cs) = coupledSubset' [] [c] cs

coupledSubsets :: Constraints -> [Constraints]
coupledSubsets [] = []
coupledSubsets pool = subset:coupledSubsets nextPool
  where
    (subset, nextPool) = coupledSubset pool

type Solution = Map.Map Pos Bool

-- Number of solutions in which a variable must = 1 (mine at that position)
-- (for a given number of mines)
data SolutionTallies = SolutionTallies {
  tallies :: Map.Map Pos Int
, solutionCount :: Int
} deriving (Show)

-- When we don't need to know how many mines solutions require
type CollapsedSolutions = SolutionTallies

combineTallies :: SolutionTallies -> SolutionTallies -> SolutionTallies
combineTallies (SolutionTallies ts n) (SolutionTallies ts2 n2) =
  SolutionTallies (Map.unionWith (+) ts ts2) (n + n2)

type SolutionSet = Map.Map Int SolutionTallies

minMines :: SolutionSet -> Int
--minMines ss | trace ("minMines " ++ show ss) False = undefined
--minMines ss = head $ Map.keys ss
minMines = head . Map.keys

maxMines :: SolutionSet -> Int
--maxMines ss | trace ("maxMines " ++ show ss) False = undefined
--maxMines ss = last $ Map.keys ss
maxMines = last . Map.keys

-- Trick for total mines by summing per group (mines used by group * solutionCount)
totalMines :: SolutionSet -> Int
totalMines = sum . map (uncurry (*) . second solutionCount) . Map.toList

-- Get the set of variables used in a solution set
ssVariables :: SolutionSet -> Set.Set Pos
ssVariables = Set.unions . map (Map.keysSet . tallies) . Map.elems

-- Number of solutions represented by the entire set
ssSolutionCount :: SolutionSet -> Int
ssSolutionCount = sum . map solutionCount . Map.elems

zipWithPair :: (a -> b -> b) -> (a' -> b' -> b') -> ((a, a') -> (b, b') -> (b, b'))
zipWithPair f f2 (x, y) = f x *** f2 y

-- Destroy grouping information
collapseSolutionSet :: SolutionSet -> CollapsedSolutions
collapseSolutionSet = foldr combineTallies (SolutionTallies Map.empty 0) . Map.elems

-- Average mines required by each solution in a collapsed set
expectedMines :: SolutionSet -> Float
expectedMines ss = fromIntegral (totalMines ss) / fromIntegral (ssSolutionCount ss)

applyVariablesAll :: Variables -> Constraints -> Constraints
applyVariablesAll vars = filter (not . isEmpty_) . map (applyVariables vars)

tryAssignment :: Variables -> Constraint -> Maybe Constraint
tryAssignment vars c
  | assigned^.rhs < 0 = Nothing
  | assigned^.isEmpty && assigned^.rhs /= 0 = Nothing
  | otherwise = Just assigned
  where
    assigned = applyVariables vars c

tryAssignments :: Variables -> Constraints -> Maybe [Constraint]
tryAssignments vars = fmap (filter (not . isEmpty_)) . mapM (tryAssignment vars)

-- getRestricted finds the variables  which are "restricted" (its value can
-- be inferred by the constraint)
getRestricted :: Constraint -> [Variable]
--getRestricted c | trace ("getRestricted " ++ show c) False = undefined
getRestricted (Constraint lhs rhs)
  -- All variables must be 0
  | rhs == 0            = lVars False
  -- All variables must be 1
  | Set.size lhs == rhs = lVars True
  | otherwise           = []
  where
    lVars v = map (, v) $ Set.toList lhs
    first = head $ Set.toList lhs

findRestricted :: Constraints -> [Variable]
findRestricted = concatMap getRestricted

-- Return the variables appearing most frequently in the provided constraints
mostConstrained :: Constraints -> [Pos]
mostConstrained cs = map fst . List.sortBy (\(_, n) (_, n2) -> compare n2 n) $ Map.toList topMap
  where
    topMap = Map.unionsWith (+) $ map (Map.fromSet (const 1) . _lhs) cs

-- nextAssignments finds the next variable to assign to and its possible values,
-- in order of:
--  - Variable whose value can be inferred
--  - The "most constrained" variable (with 0 and then 1)
--nextAssignments :: Constraints -> Variables -> [Variables]
nextAssignments :: Constraints -> Variables -> (Pos, [Bool])
--nextAssignments cs vars | trace ("nextAssignments " ++ show cs ++ " " ++ show vars) False = undefined
nextAssignments cs vars =
  case findRestricted cs of
    (p, v):_ -> (p, [v])
    []       -> (most, [False, True])
    where
      most = head . filter (not . (`Map.member` vars)) $ mostConstrained cs

ssItem :: Solution -> (Int, SolutionTallies)
ssItem s = (sumWith btoi s, SolutionTallies { tallies = Map.map btoi s, solutionCount = 1 } )

makeSolutionSet :: [Solution] -> SolutionSet
makeSolutionSet ss = Map.fromListWith combineTallies (map ssItem ss)

solutions' :: Constraints -> Variables -> [Solution]
-- succeeded all the way down, solution found!
--solutions' cs vars | trace ("solutions " ++ show cs ++ " " ++ show vars) False = undefined
solutions' [] vars = [vars]
solutions' cs vars =
  -- reduce the constraints by applying variables
  case tryAssignments vars cs of
    -- assignment succeeded, go deeper!
    Just ncs -> concatMap (solutions' ncs) nvars
    Nothing -> []
  where
    -- find the next potential assignment(s)
    (nvar, nvalues) = nextAssignments cs vars
    nvars = map (\v -> Map.insert nvar v vars) nvalues

solutions :: Constraints -> SolutionSet
solutions cs = makeSolutionSet $ solutions' cs Map.empty

nonEmpty :: [SolutionSet] -> [SolutionSet]
nonEmpty = filter (not . Map.null)

-- removeInfeasible filters solution sets by removing from each set those
-- solutions which require too few or too many mines in combination with other
-- sets.
-- For example, if a group of solutions in a set require 2 mines, and all the
-- groups in two other sets require at minimum 3 and 4 respectively, but there
-- are only 8 mines left, the group of 2 will be dropped from that set
removeInfeasible :: Int -> [SolutionSet] -> [SolutionSet]
--removeInfeasible total sss | trace ("removeInfeasible " ++ show sss) False = undefined
removeInfeasible total sss = nonEmpty $ map mapper indexed
  where
    indexed = zip [0..] sss

    mapper (i, ss) = Map.filterWithKey (\n _ -> n >= newMin && n <= newMax) ss
      where
        notCurrent = (/=) i . fst

        minRest  = sumWith (minMines . snd) $ filter notCurrent indexed
        maxRest  = sumWith (maxMines . snd) $ filter notCurrent indexed

        --minRest = trace ("minRest " ++ show minRest') minRest'
        --maxRest = trace ("maxRest " ++ show maxRest') maxRest'

        newMin = maxRest
        newMax = total - minRest

-- findCertain fines any variables whose values can be inferred (either 1 or 0
-- in all solutions)
findCertain :: CollapsedSolutions -> [Variable]
findCertain css = Map.toList . Map.map itob . Map.filter (sCount ==) $ tallies css
  where
    sCount = solutionCount css

-- A "crap shoot" move is a 50/50 guess which occurs when all solutions require
-- the same number of mines and all neighbours for variables are known
crapShoot :: CSPState -> SolutionSet -> Maybe Move
crapShoot m ss
  | Map.size ss == 1 && allKnown = Just $ Move CrapShoot p probMine
  | otherwise = Nothing
  where
    sts = head $ Map.elems ss
    ts = tallies sts

    ns = concatMap (Set.toList . neighbours (_boardSize m)) $ Map.keys ts
    allKnown = all (`Map.member` _variables m) ns

    (p, n) = List.minimumBy (\(_, n) (_, n2) -> compare n n2) $ Map.toList ts
    probMine = fromIntegral n / fromIntegral (solutionCount sts)

-- Find the probabilities there's a mine for every variable (certainties are filtered out!)
guesses :: CollapsedSolutions -> [Move]
--guesses ss = List.sort $ concatMap movesForGroup ss
guesses css = List.sort . map (uncurry (Move Guess)) . filter ((>0) . snd) . Map.toList . Map.map countToProbability $ tallies css
  where
    sCount = solutionCount css
    -- P(mine) = solutions with mine / total solutions
    countToProbability n = fromIntegral n / fromIntegral sCount

-- The probability an unconstrained square has a mine
nonConstrainedChance :: Int -> [SolutionSet] -> Float
nonConstrainedChance remaining sss = (rf - sum (map expectedMines sss)) / rf
  where rf = fromIntegral remaining

corners :: Bounds -> [Pos]
corners (width, height) = [(0, 0), (width-1, 0), (0, height-1), (width-1, height-1)]

edges :: Bounds -> [Pos]
edges (width, height) = concat [
    [(x,      0)      | x <- xs]
  , [(0,      y)      | y <- ys]
  , [(x,      height-1) | x <- xs]
  , [(width-1, y)      | y <- ys]
  ]
  where
    xs = [1..width-2]
    ys = [1..width-2]

outer :: Bounds -> [Pos]
outer bs = corners bs ++ edges bs

interior :: Bounds -> [Pos]
interior (width, height) = [(x, y) | x <- [1..width-2], y <- [1..height-2]]

-- A list of "uneducated" guesses to make, in the following order:
--  - Corners
--  - Edges
--  - "Boundary" squares (constrained squares in order of most used to least)
--  - Any other interior squares
weakHeuristic :: CSPState -> [Pos]
weakHeuristic m = concat [
    withoutBoundary $ outer bs
  , Set.toList boundary
  , withoutBoundary $ interior bs
  ]
  where
    bs = _boardSize m
    cs = _constraints m

    vars = _variables m
    boundary = Set.fromList $ mostConstrained cs
    withoutBoundary = filter (\p -> not $ p `Set.member` boundary)

-- Stream of possible moves that can be made in the current state, ordered in
-- ascending probability of a mine. Duplicates may be present! Also returns any
-- positions which were inferred to be mines

-- Might return so-called  so-called "crap shoots", which should be played first
-- to get toss up guesses out of the way)
moves :: CSPState -> ([Move], [Pos])
moves m = (
    filter (not . (`Set.member` _played m) . _movePos) $ concat [
      vars'
    , certainSafe
    , crapShoots
    , guesses'
    , heuristic
    ]
  , flags
  )
  where
    -- Global state shortcuts
    vars = _variables m
    cs = _constraints m

    -- Intermediates
    rms = remainingMines m
    subsets = coupledSubsets cs
    sss' = nonEmpty $ solutions <$> subsets
    sss = removeInfeasible rms sss'
    --sss = trace ("with infeasiable " ++ show sss') sss_
    --sss = trace ("final " ++ show sss_) sss_
    ncProb = nonConstrainedChance rms sss
    csss = collapseSolutionSet <$> sss
    --csss = trace ("collapsed " ++ show csss_) csss_

    certainVars = concat $ findCertain <$> csss

    -- Final values
    vars' = map (\p -> Move KnownVariable p 0) . Map.keys $ Map.filter not vars
    (flags, certainSafe) = second (map (\p -> Move CertainSafe p 0)) . both (map fst) $ List.partition snd certainVars
    crapShoots = crapShoot m `mapMaybe` sss
    guesses' = List.sort . concat $ guesses <$> csss
    heuristic = (\p -> Move Heuristic p ncProb) <$> weakHeuristic m

runMinesweeper :: CSP a -> CSPState -> (a, CSPState)
runMinesweeper = runState

evalMinesweeper :: CSP a -> CSPState -> a
evalMinesweeper = evalState

execMinesweeper :: CSP a -> CSPState -> CSPState
execMinesweeper = execState
