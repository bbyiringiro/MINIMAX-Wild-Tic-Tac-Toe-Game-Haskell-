
module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6


{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = [(1,3)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth=23
-- Why did you choose this number?
-- YOUR ANSWER GOES HERE
-- As maxDepth number is only needed in iterDeepSearch function and although it employs deepfirst search algorithm we know that it is optimal because before it increment the next depth(d), it first searches all possible braches of the tree up to that depth(d).
--  Therefore it will find the shortest path form start to the goal. So the only way  to stop if from using the straigh shortest manhattan distance to the goal is if there are  bad nodes in between. So I tried to come up with the worst case  configuration of bads nodes in the grid that will make longest shortest path you can find the 6x6 grid
-- Using the following similation, assuming that S cell is where an agent starts and X's are bad nodes and T is target goal, and dot(.) is free cell, is the worst case configuration of bad nodes that will make largest maxdepth for iterDeepSearch.
-- . . . . . .
-- S X X X X .
-- X . . . . .
-- . . X X X X
-- . X . . . X
-- . . . X . T

-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  [] -- next called with an empty list returns an empty list
next branch | head branch `elem` badNodesList = []  --- when the start node is bad node
            | otherwise = [ headNode:branch
                | headNode <- continuations_poss,
                            isValid headNode, -- check if it is in the grid scope
                            headNode `notElem` branch -- prevent loop in the path
                            && headNode `notElem` badNodesList] -- check if the next possible nodes are already explored or in bad nodes
                where
                -- robot's head
                (i, j) = head branch
                -- computing the next possible locations
                continuations_poss = [(i, j+1),(i, j-1),(i+1, j),(i-1, j)]


-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode -- check if the destination is equal to the current node


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch destination next branches exploredList
  | goalFound = Just (head $ filter  (\path-> checkArrival destination (head path)) flontier)
  | null flontier = Nothing -- returns nothing when no branches to explore or when flontier is empty
  | otherwise = breadthFirstSearch destination next flontier (exploredList ++ heads) -- recurse with new flontier and add explored nodes to the list
    where
      flontier::[Branch]
      flontier = filter (\xs-> head xs `notElem` exploredList) $ concatMap next branches -- filter the expansation of the flontier and make sure you remove nodes you already explored.
      heads::[Node]
      heads = map head flontier
      goalFound = any (\x->checkArrival destination x) heads  -- check if the new children there is a target goal






-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.



depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next branches exploredList = loop branches
  where
    loop [] = Nothing -- return nothing if nothing to on flontier
    loop (b:bs) -- this loop helps us similate stack data stracture as it does recurse of first child way down, until it goes to the next nodes on the same level of the tree
        | checkArrival destination (head b) = Just (b)
        | recursive_dls == Nothing = loop bs
        | otherwise = recursive_dls
        where flontier = filter (\xs-> head xs `notElem` exploredList) $ next b -- remove prevent to explore nodes we have already explored. However exploredList is not global
              recursive_dls = depthFirstSearch destination next flontier ([head b] ++ exploredList)



-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch destination next branches d = loop branches
  where
    loop [] = Nothing -- returns nothing when no branches to explore
    loop (b:bs)
        | checkArrival destination (head b) = Just (b)
        | d ==0 = Nothing
        | recursive_dls == Nothing = loop bs
        | otherwise = recursive_dls
        where getfFlontier = next -- as it depthLimed it is not necessary to keep track of the expolored list.
              recursive_dls = depthLimitedSearch destination next (getfFlontier b) (d-1)




-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
  | found /= Nothing = found
  | d > maxDepth = Nothing
  | found == Nothing = iterDeepSearch destination next initialNode (d+1)
    where
      found = depthLimitedSearch destination next [[initialNode]] d

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan position destination = abs(fst position - fst destination) + abs(snd position - snd destination)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList
  | goal /= [] = Just (head goal)
  | null sortedBranches = Nothing
  | otherwise = bestFirstSearch destination next heuristic sortedBranches (exploredList ++ heads)
  where
  sortfx branchA branchB = compare (heuristic (head branchA)) (heuristic (head branchB)) -- this function help helps us implement priority queue. And sorts nodes to explore depedning on heuristic passed.
  sortedBranches::[Branch]
  sortedBranches = sortBy sortfx $ filter (\xs-> head xs `notElem` exploredList) $ concatMap next branches
  goal = (filter (\branch -> checkArrival destination (head branch)) sortedBranches)
  heads = map head sortedBranches

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList
  | goal /= [] = Just (head goal)
  | null sortedBranches = Nothing
  | otherwise = aStarSearch destination next heuristic cost sortedBranches (exploredList ++ heads)
  where
    getTotalCost::Branch->Int
    getTotalCost  x= cost x + heuristic (head x)
    sortfx branchA branchB = compare (getTotalCost branchA) (getTotalCost branchB) -- the function does exact the smae thing as bestFirstSearch expect heuristic function is defferent
    sortedBranches::[Branch]
    sortedBranches = sortBy sortfx $ filter (\xs-> head xs `notElem` exploredList) $ concatMap next branches
    goal = (filter (\branch -> checkArrival destination (head branch)) sortedBranches)
    heads = map head sortedBranches


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
    | checkWin game humanPlayer = 1 -- we would check if the game state is terminal, however I omitted this since we call terminal function before we call this function
    | checkWin game compPlayer = -1
    | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player
    | terminal game = eval game
    | otherwise = findUtility $ map (\g-> minimax g $ switch player) (moves game player) -- recuse through the whole posisibities until the leave that give utility values
      where
          findUtility = if maxPlayer player then maximum else minimum -- maximazing utility if you are Max or otherwise minimise it



-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Game->Player->Int
alphabeta game player = alphabetaInit game (-2) 2 -- +-2 replaces & -& since utility value is 1, 0, or -1 \x\<=1
    where
        alphabetaInit = if maxPlayer player then maxValue else minValue -- determine who did the move and call min or max accordingly

        maxValue state alpha beta
            | terminal state = eval state
            | otherwise = maxVLoop (moves state humanPlayer) alpha beta (-2)

        minValue state alpha beta
            | terminal state = eval state
            | otherwise = minVLoop (moves state compPlayer) alpha beta 2

        maxVLoop [] _ _ v = v -- g is for game state, and gs for games states
        maxVLoop (g:gs) alpha beta v
            | changedV >= beta = changedV
            | otherwise = maxVLoop gs (max changedV alpha) beta changedV
            where changedV = max v $ minValue g alpha beta


        minVLoop [] _ _ v = v
        minVLoop (g:gs) alpha beta v -- -- g is for game state, and gs for games states
            | changedV <= alpha = changedV
            | otherwise = minVLoop gs alpha (min changedV beta) changedV
            where changedV = min v $ maxValue g alpha beta


-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
 | checkWin game 1 = 1 -- we would check if the game state is terminal, however I omitted this since we call terminal function before we call this function
 | checkWin game 0 = 1
 | otherwise = 0



-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

--


alphabetaWild:: Game->Player->Int
alphabetaWild game player = alphabetaInit player game (-2) 2 -- +-2 replaces & -& since utility value is 1, 0, or -1 \x\<=1
    where
        alphabetaInit = if maxPlayer player then maxValue else minValue

        maxValue p state alpha beta
            | terminal state = (-1) * evalWild state -- check the player who put the game to terminal state and reward the utility accordingly .. it would be min in this case
            | otherwise = maxVLoop humanPlayer (movesWild state humanPlayer) alpha beta (-2)

        minValue p state alpha beta
            | terminal state =  evalWild state -- check the player who put the game to terminal state and reward the utility accordingly .. in this case it would be max
            | otherwise = minVLoop compPlayer (movesWild state compPlayer) alpha beta 2

        maxVLoop _ [] _ _ v = v
        maxVLoop p (g:gs) alpha beta v -- g is for game state, and gs for games states
            | changedV >= beta = changedV
            | otherwise = maxVLoop p gs (max changedV alpha) beta changedV
            where changedV = max v $ minValue p g alpha beta

        minVLoop _ [] _ _ v = v
        minVLoop p (g:gs) alpha beta v -- -- g is for game state, and gs for games states
            | changedV <= alpha = changedV
            | otherwise = minVLoop p gs alpha (min changedV beta) changedV
            where changedV = min v $ maxValue p g alpha beta


-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player
    | terminal game = if maxPlayer player then  evalWild game else (-1) * evalWild game
    | otherwise = utilityFx $ map (\game-> minimax game $ switch player) (movesWild game player)
      where
          utilityFx = if maxPlayer player then maximum else minimum



			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores

-- it checks if an expansion of a node is valid
isValid :: Node -> Bool
isValid (x, y) = and [x >= 1, y >= 1, x <= gridWidth_search, y <= gridLength_search]
