--By Jonathan Mercer and Elizabeth Brooks 


-- crusher_i8r7
-- 
-- Generates a new crusher move based on searching the tree of future moves.
-- 
-- Arguments:
--  - prevBoards: the input list of boards
--  - side: which side you're moving
--  - depth: search tree depth
--  - boardSize: length of the side of the hexigonal board
-- Returns: new move ++ prevBoards
crusher_i8r7 :: [String] -> Char -> Int -> Int -> [String]
crusher_i8r7 (currentBoard:boardHistory) side depth boardSize = 
    main_i8r7 (boardParser_i8r7 currentBoard boardSize 0 boardSize)
        (currentBoard:boardHistory) side depth boardSize

-- boardParser_i8r7
-- 
-- -- cuts board into a list of lines of the board
-- 
-- Arguments:
     -- origBoard: the board we're making moves from
     -- n: board size iterator
     -- y: iterates through the lines of the board
     -- origN: board size
-- Returns:
--      parsed board where we've cut up the board into a list lines
boardParser_i8r7 :: String -> Int -> Int -> Int -> [String]
boardParser_i8r7 origBoard n y origN
    | null origBoard            = []
    | y < (origN - 1)           = (take n origBoard):
        (boardParser_i8r7 (drop n origBoard) (n + 1) (y + 1) origN)
    | y == (origN - 1)          = (take n origBoard):
        (boardParser_i8r7 (drop n origBoard) (n - 1) (y + 1) origN)
    | y > (origN - 1)           = (take n origBoard):
        (boardParser_i8r7 (drop n origBoard) (n - 1) (y + 1) origN)
        
-- unparseBoard_i8r7
-- 
-- -- sticks parsed board back together
-- 
-- Arguments:
     -- board: a parsed board
-- Returns:
--      unparsed board
unparseBoard_i8r7 :: [String] -> String
unparseBoard_i8r7 board 
    | null board        = ""
    | otherwise         = (head board) ++ 
        (unparseBoard_i8r7 (tail board))

-- main_i8r7
-- 
--  appends ChooseMove to the rest of the board history
-- 
-- Arguments:
--  - currentBoard: the input list of boards
--  - boardHistory: list of String
--  - side: which side you're moving
--  - depth: search tree depth
--  - boardSize: length of the side of the hexigonal board
-- Returns: new move ++ prevBoards
--
main_i8r7 :: [String] -> [String] -> Char -> Int -> Int -> [String]
main_i8r7 currentBoard boardHistory side depth boardSize =
    (chooseMove_i8r7 currentBoard boardHistory side depth boardSize):
    boardHistory

-- chooseMove_i8r7
-- 
--  sticks back together the new move's board
-- 
-- Arguments:
--  - currentBoard: the input list of boards
--  - boardHistory: list of String
--  - side: which side you're moving
--  - depth: search tree depth
--  - boardSize: length of the side of the hexigonal board
-- Returns: new move ++ prevBoards
chooseMove_i8r7 :: [String] -> [String] -> Char -> Int -> Int -> String
chooseMove_i8r7 currentBoard boardHistory side depth boardSize = 
    unparseBoard_i8r7 
        (searchTheTree_i8r7
            (generateTree_i8r7 boardHistory side 1 depth boardSize [(currentBoard,0,[])]))











-- generateTree_i8r7
-- 
--  makes a tree of nodes (board,heuristic,list of children) of the possible next moves
-- (stops at given search depth) 
-- 
-- Arguments:
--  - boardHistory: list of String
--  - side: which side you're moving
--  - depth: search tree depth counter
--  - origDepth: search tree depth
--  - boardSize: length of the side of the hexigonal board
--  - tree: starts with the parsed currentBoard in it
-- Returns: new tree
generateTree_i8r7 :: [String] -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
generateTree_i8r7 boardHistory side depth origDepth boardSize tree
    | depth == origDepth    = generateNewChildren_i8r7 boardHistory side origDepth boardSize tree depth tree 
    | otherwise             = generateTree_i8r7 
        boardHistory 
        (changeSide_i8r7 side) 
        (depth + 1) 
        origDepth 
        boardSize 
        (generateNewChildren_i8r7 boardHistory side origDepth boardSize tree depth tree)

-- changeSide_i8r7
-- 
-- -- switches the given side to the opposite side
-- 
-- Arguments:
     -- side: original side
-- Returns:
--      opposite side
changeSide_i8r7 :: Char -> Char
changeSide_i8r7 side
    | side == 'W'   = 'B'
    | otherwise     = 'W'

-- generateNewChildren_i8r7
-- 
--  takes out each node and appends the node and the node's subtree in the node's place
-- 
-- Arguments:
--  - boardHistory: list of String
--  - side: which side you're moving
--  - depth: search tree depth counter
--  - origDepth: search tree depth
--  - boardSize: length of the side of the hexigonal board
--  - tree: has all all previously generated moves in it
-- Returns: the tree with another layer of children added on to it
--
generateNewChildren_i8r7 :: [String] -> Char -> Int -> Int -> [([String],Int,[[String]])] -> Int -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
generateNewChildren_i8r7 boardHistory side origDepth boardSize tree depth fullTree
    | null tree                             = []
    | not(hasChildren_i8r7 (head tree))     = (generateNewMoves_i8r7 [(head tree)] boardHistory side origDepth boardSize depth fullTree) ++ 
        (generateNewChildren_i8r7 boardHistory side origDepth boardSize (tail tree) depth fullTree)
    | otherwise                             = (head tree) : 
        (generateNewChildren_i8r7 boardHistory side origDepth boardSize (tail tree) depth fullTree)
-- TODO: check heuristic of current node before making subtree: if losing or winning board, make no children. (just re-append node to tree by itself)
        
        
-- generateNewMoves_i8r7
-- 
--  calls appendChildren_i8r7 with generatePossMoves_i8r7's result as a parameter and the root-only subTree
-- 
-- Arguments:
--  - subTree: starts with the node to generate moves from
--  - boardHistory: list of String
--  - side: which side you're moving
--  - origDepth: search tree depth
--  - boardSize: length of the side of the hexigonal board
-- Returns: the sub tree of new moves from the node given
--  
generateNewMoves_i8r7 :: [([String],Int,[[String]])] -> [String] -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
generateNewMoves_i8r7 subTree boardHistory side origDepth boardSize depth tree =
    appendChildren_i8r7 (generatePossMoves_i8r7 
        (head subTree) boardHistory side origDepth boardSize depth tree) subTree

-- appendChildren_i8r7
-- 
--  sticks root node with new children onto list of new children nodes
-- 
-- Arguments:
--  - newMoves: list of nodes possible
--  - subTree: starts with the node to generate moves from
-- Returns: the sub tree with the new moves appended
--  
appendChildren_i8r7 :: [([String],Int,[[String]])] -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
appendChildren_i8r7 newMoves subTree =
    (newRoot_i8r7 newMoves (head subTree) []) : newMoves 

-- newRoot_i8r7
-- 
--  adds the board of each new move onto the root node's list of children
-- 
-- Arguments:
--  - newMoves: list of nodes possible
--  - parentNode: the parent node
--  - newChildren: list of the parent's children
-- Returns: root node with new move children added to root's children list
--  
newRoot_i8r7 :: [([String],Int,[[String]])] -> ([String],Int,[[String]]) -> [[String]] -> ([String],Int,[[String]])
newRoot_i8r7 newMoves (board,h,children) newChildren 
    | null newMoves                 = (board,h,newChildren)
    | otherwise                     = newRoot_i8r7 (tail newMoves) (board,h,children) (addChild_i8r7 (head newMoves) newChildren)

-- addChild_i8r7
-- 
--  adds the new move's board to the parent's list of children
-- 
-- Arguments:
--  - newMove: one possible move's node
--  - newChildren: list of the parent's children
-- Returns: the parent's list of children with this move's board added
--  
addChild_i8r7 :: ([String],Int,[[String]]) -> [[String]] -> [[String]]
addChild_i8r7 (board,h,loc) newChildren = 
    board:newChildren










-- generatePossMoves_i8r7
-- 
--  sricking together the results of generateNewSlides_i8r7 and generateNewHops_i8r7
-- 
-- Arguments:
--  - startNode: the node to generate moves from
--  - boardHistory: list of String
--  - side: which side you're moving
--  - origDepth: search tree depth
--  - boardSize: length of the side of the hexigonal board
-- Returns: The list of possible moves from 
--  
generatePossMoves_i8r7 :: ([String],Int,[[String]]) -> [String] -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
generatePossMoves_i8r7 (origBoard,_,_) boardHistory side origDepth boardSize depth tree = 
    (generateNew_i8r7 origBoard origBoard boardHistory side origDepth boardSize 0 depth tree)

-- generateNew_i8r7
-- 
--  goes through each row and and calls findMovableTileInLine_i8r7
-- 
-- Arguments:
--  - chopBoard: the board to take the head and tail from
--  - origBoard: the board to generate moves from
--  - boardHistory: list of String
--  - side: which side you're moving
--  - origDepth: search tree depth
--  - boardSize: length of the side of the hexigonal board
--  - y: the line nuber, starting at 0
-- Returns: The list of possible moves from 
--  
generateNew_i8r7 :: [String] -> [String] -> [String] -> Char -> Int -> Int -> Int -> Int -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
generateNew_i8r7 chopBoard origBoard boardHistory side origDepth boardSize y depth tree
    | null chopBoard        = []
    | otherwise             = (findMovableTileInLine_i8r7 (head chopBoard) origBoard boardHistory side origDepth boardSize 0 y depth tree) ++ 
        (generateNew_i8r7 (tail chopBoard) origBoard boardHistory side origDepth boardSize (y + 1) depth tree)

-- findMovableTileInLine_i8r7
-- 
--  finds the movable tiles and calls moveTile_i8r7
-- 
-- Arguments:
--  - line: the current line of the board
--  - origBoard: the board to generate moves from
--  - boardHistory: list of String
--  - side: which side you're moving
--  - origDepth: search tree depth
--  - boardSize: length of the side of the hexigonal board
--  - x: place in line length
--  - y: place in board height
-- Returns: The list of possible moves from 
--  
findMovableTileInLine_i8r7 :: String -> [String] -> [String] -> Char -> Int -> Int -> Int -> Int -> Int -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
findMovableTileInLine_i8r7 line origBoard boardHistory side origDepth boardSize x y depth tree
    | null line                 = []
    | (head line) == side       = (moveTile_i8r7 x y origBoard boardHistory side origDepth boardSize depth tree) ++ 
        (findMovableTileInLine_i8r7 (tail line) origBoard boardHistory side origDepth boardSize (x + 1) y depth tree)
    | otherwise                 = findMovableTileInLine_i8r7 (tail line) origBoard boardHistory side origDepth boardSize (x + 1) y depth tree







-- moveTile_i8r7
-- 
-- -- makes the nodes for each possible move from the current board
-- 
-- Arguments:
     -- x: the position in line
     -- y: the line in board
     -- origBoard: a parsed board
     -- boardHistory: list of string
     -- side: current size
     -- origDepth: given search depth
     -- boardSize: size of board
     -- depth: current search depth
     -- tree: list of nodes
-- Returns:
--      list of possible node moves from origBoard
moveTile_i8r7 :: Int -> Int -> [String] -> [String] -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
moveTile_i8r7 x y origBoard boardHistory side origDepth boardSize depth tree = 
    (assembleNewNodes_i8r7 (newSlideCoords_i8r7 x y origBoard side) x y origBoard boardHistory side origDepth boardSize depth tree) ++
    (assembleNewNodes_i8r7 (newHopCoords_i8r7 x y origBoard side) x y origBoard boardHistory side origDepth boardSize depth tree)



-- newHopCoords_i8r7
-- 
-- -- gets possible legal hop coordinates
-- 
-- Arguments:
     -- x: the position in line
     -- y: the line in board
     -- origBoard: a parsed board
     -- side: current size
-- Returns:
--      returns the possible hop coordinates
newHopCoords_i8r7 :: Int -> Int -> [String] -> Char -> [(Int,Int)]
newHopCoords_i8r7 x y origBoard side = 
    (newCoordinates_i8r7 x y origBoard side 2)

-- newSlideCoords_i8r7
-- 
-- -- gets possible legal slide coordinates
-- 
-- Arguments:
     -- x: the position in line
     -- y: the line in board
     -- origBoard: a parsed board
     -- side: current size
-- Returns:
--      the possible slide coordinates
newSlideCoords_i8r7 :: Int -> Int -> [String] -> Char -> [(Int,Int)]
newSlideCoords_i8r7 x y origBoard side = 
    (newCoordinates_i8r7 x y origBoard side 1)

-- assembleNewNodes_i8r7
-- 
-- --Implements changes suggested by slidCoordinates
-- 
-- Arguments:
     -- coordinates: new possible move coordinates
     -- x: position in line
     -- y: line in board
     -- origBoard: a parsed board
     -- boardHistory: list of string
     -- side: current side
     -- origDepth: given search depth
     -- boardSize: size of board
     -- depth: current depth
     -- tree: list of nodes
-- Returns:
--      list of new nodes
assembleNewNodes_i8r7 :: [(Int,Int)] -> Int -> Int -> [String] -> [String] -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> [([String],Int,[[String]])]
assembleNewNodes_i8r7 coordinates x y origBoard boardHistory side origDepth boardSize depth tree
    | null coordinates      = []
    | otherwise             = (makeNode_i8r7 (moveBoard_i8r7 (head coordinates) x y origBoard side) 
        boardHistory side origDepth boardSize depth tree) : 
        assembleNewNodes_i8r7 (tail coordinates) x y origBoard boardHistory side origDepth boardSize depth tree

-- moveBoard_i8r7
-- 
-- -- Replace current position with "-" and replace coordinate postion with tile side
-- 
-- Arguments:
     -- coordinates: new possible move coordinates
     -- x: the position in line
     -- y: the line in board
     -- origBoard: a parsed board
     -- side: current size
-- Returns:
--      the new move board
moveBoard_i8r7 :: (Int,Int) -> Int -> Int -> [String] -> Char -> [String]
moveBoard_i8r7 coordinate x y origBoard side =
    replaceTile_i8r7 (replaceTile_i8r7 origBoard '-' (x,y)) side coordinate 

-- replaceTile_i8r7
-- 
-- -- help find the spot where you want to replace a character
-- 
-- Arguments:
     -- origBoard: a parsed board
     -- repl: replacement tile
     -- (replX,replY): the replacement tile coordinates
-- Returns:
--      a board with a replaced tile
replaceTile_i8r7 :: [String] -> Char -> (Int,Int) -> [String]
replaceTile_i8r7 origBoard repl (replX,replY)
    | replY == 0        = (replTileInLine_i8r7 (head origBoard) repl replX) : (tail origBoard)
    | otherwise         = (head origBoard) : (replaceTile_i8r7 (tail origBoard) repl (replX, (replY - 1)))

-- replTileInLine_i8r7
-- 
-- -- Replace the x position with the replacement character
-- 
-- Arguments:
     -- line: current line
     -- repl: replacement tile
     -- replX: replacement tile position in line
-- Returns:
--      line with replaced tile
replTileInLine_i8r7 :: String -> Char -> Int -> String
replTileInLine_i8r7 line repl replX
    | replX == 0        = repl : (tail line)
    | otherwise         = (head line) : (replTileInLine_i8r7 (tail line) repl (replX - 1))

-- makeNode_i8r7
-- 
-- -- puts together a node from paramenters given
-- 
-- Arguments:
     -- board: a parsed board
     -- boardHistory: list of string
     -- side: current side
     -- origDepth: given search depth
     -- boardSize: size of board
     -- depth: current search depth
     -- tree: list of nodes
-- Returns:
--      a node
makeNode_i8r7 :: [String] -> [String] -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> ([String],Int,[[String]])
makeNode_i8r7 board boardHistory side origDepth boardSize depth tree = 
    (board, (heuristic_i8r7 board boardHistory side depth origDepth boardSize tree), [])

-- heuristic_i8r7
-- 
-- -- calls heuristicHelper_i8r7 to get heuristic
-- 
-- Arguments:
     -- board: a parsed board
     -- boardHistory: list of string
     -- side: current side
     -- origDepth: given search depth
     -- boardSize: size of board
     -- depth: current search depth
     -- tree: list of nodes
-- Returns:
--      the heuristic of the node
heuristic_i8r7 :: [String] -> [String] -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> Int 
heuristic_i8r7 board boardHistory side depth origDepth boardSize tree = 
    heuristicHelper_i8r7 board boardHistory side (getOrigSide_i8r7 depth side) depth origDepth boardSize tree

-- getOrigSide_i8r7
-- 
-- -- gets the original side given to crusher_i8r7 by using the tree depth and current side
-- 
-- Arguments:
     -- depth: current search depth
     -- side: currend side
-- Returns:
--      original side given to crusher
getOrigSide_i8r7 :: Int -> Char -> Char
getOrigSide_i8r7 depth side
    | isEven_i8r7 depth         = side
    | otherwise                 = (changeSide_i8r7 side)

-- heuristicHelper_i8r7
-- 
-- -- calls determineHeurisitic_i8r7 based on what the current side compared to the original side
-- 
-- Arguments:
     -- board: a parsed board
     -- boardHistory: list of string
     -- side: current side
     -- origSide: the original side given
     -- depth: current search depth
     -- origDepth: given search depth
     -- boardSize: size of board
     -- tree: list of nodes
-- Returns:
--      the heuristic of the node
heuristicHelper_i8r7 :: [String] -> [String] -> Char -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> Int 
heuristicHelper_i8r7 board boardHistory side origSide depth origDepth boardSize tree
    | (side == origSide)        = determineHeuristic_i8r7 board boardHistory side origSide depth origDepth boardSize tree
    | otherwise                 = determineHeuristic_i8r7 board boardHistory side origSide depth origDepth boardSize tree

-- determineHeuristic_i8r7
-- 
-- -- determines the board heuristic
-- 
-- Arguments:
     -- board: a parsed board
     -- boardHistory: list of string
     -- side: current side
     -- origSide: the original side given
     -- depth: current search depth
     -- origDepth: given search depth
     -- boardSize: size of board
     -- tree: list of nodes
-- Returns:
--      the heuristic of the node
determineHeuristic_i8r7 :: [String] -> [String] -> Char -> Char -> Int -> Int -> Int -> [([String],Int,[[String]])] -> Int
determineHeuristic_i8r7 board boardHistory side origSide depth origDepth boardSize tree
    | (isLose_i8r7 side origSide board boardHistory boardSize)      = (addDepth_i8r7 (-(boardSize * origDepth) - depth) depth)
    | (isWin_i8r7 side origSide board boardHistory boardSize)       = (addDepth_i8r7 ((boardSize * origDepth) + depth) depth)
    | otherwise                                                     = 
        (addDepth_i8r7 (((tileCount_i8r7 origSide board) - (tileCount_i8r7 side board)) + depth) depth)
-- TODO: check about hops????

-- cannotMove_i8r7
-- 
-- --Finds all coordinates of "side" in a board and passes it to the helper
-- 
-- Arguments:
     -- side: current side
     -- board: parsed board
-- Returns:
--      true if no possible moves false otherwise
cannotMove_i8r7 :: Char -> [String] -> Bool
cannotMove_i8r7 side board = 
    cannotMove_helper_i8r7 side board (getSideCoords_i8r7 side board 0)

-- cannotMove_helper_i8r7
-- 
-- --Checks if coordsToCheck has any legal moves
-- 
-- Arguments:
     -- side: current side
     -- board: parsed board
     -- coordsToCheck: possible coordinates of moves
-- Returns:
     -- legal coordinates
cannotMove_helper_i8r7 :: Char -> [String] -> [(Int,Int)] -> Bool
cannotMove_helper_i8r7 side board coordsToCheck
    | null coordsToCheck        = True
    | not(null (findLegalCoords_i8r7 side board (head coordsToCheck)))  = False
    | otherwise                 = cannotMove_helper_i8r7 side board (tail coordsToCheck)
--The second guard can be compressed down to the third guard by using &&

-- getSideCoords_i8r7
-- 
-- --splits the board into lines
-- 
-- Arguments:
     -- side: current side
     -- board: parsed board
     -- y: current line in board
-- Returns:
--      list of possible move coordinates of current side
getSideCoords_i8r7 :: Char -> [String] -> Int -> [(Int,Int)]
getSideCoords_i8r7 side board y
    | null board        = []
    | otherwise         = (getLineCoord_i8r7 side (head board) y 0) ++ (getSideCoords_i8r7 side (tail board) (y+1))

-- getLineCoord_i8r7
-- 
-- --returns all x positions of all the "sides" on the line
-- 
-- Arguments:
     -- side: current side
     -- line: a substring of board
     -- y: current line in board
     -- x: current position in line
-- Returns:
--      all coordinates of tiles of current side in line
getLineCoord_i8r7 :: Char -> String -> Int -> Int -> [(Int,Int)]
getLineCoord_i8r7 side line y x
    | null line             = []
    | (head line) == side   = [(x,y)] ++ (getLineCoord_i8r7 side (tail line) y (x+1))
    | otherwise             = getLineCoord_i8r7 side (tail line) y (x+1)
    
-- findLegalCoords_i8r7
-- 
-- --Gives correct (legal) values
-- 
-- Arguments:
     -- side: current side
     -- board: parsed board
     -- (x,y): coordinates to move from
-- Returns:
     -- all legal move coordinates
findLegalCoords_i8r7 :: Char -> [String] -> (Int,Int) -> [(Int,Int)]
findLegalCoords_i8r7 side board (x,y) = 
    (newSlideCoords_i8r7 x y board side) ++ (newHopCoords_i8r7 x y board side)


-- isLose_i8r7
-- 
-- -- is this board a lose for original side?
-- 
-- Arguments:
     -- side: current side
     -- origSide: original side given
     -- board: parsed board
     -- boardHistory: list of string
     -- boardSize: size of board
-- Returns:
--      true if losing board for original side
isLose_i8r7 :: Char -> Char -> [String] -> [String] -> Int -> Bool
isLose_i8r7 side origSide board boardHistory boardSize
    | (sameAsHistory_i8r7 (unparseBoard_i8r7 board) boardHistory)   = True
    | (tooFewTiles_i8r7 origSide board boardSize)                   = True
    | (cannotMove_i8r7 side board)&&(side == origSide)              = True
    | otherwise                                                     = False

-- sameAsHistory_i8r7
-- 
-- -- checks if board is the same as any board in the board history
-- 
-- Arguments:
     -- board: parsed board
     -- boardHistory: size of board
-- Returns:
--      true if board is in board history
sameAsHistory_i8r7 :: String -> [String] -> Bool
sameAsHistory_i8r7 board boardHistory 
    | null boardHistory                 = False
    | board == (head boardHistory)      = True
    | otherwise                         = False || (sameAsHistory_i8r7 board (tail boardHistory))

-- tooFewTiles_i8r7
-- 
-- -- checks if there are too few tiles (a loss) of the given side on the board
-- 
-- Arguments:
     -- side: current side
     -- board: parsed board
     -- boardSize: size of board
-- Returns:
     -- true if current side tile count less than board size
tooFewTiles_i8r7 :: Char -> [String] -> Int -> Bool
tooFewTiles_i8r7 side board boardSize = 
    (tileCount_i8r7 side board) < boardSize

-- tileCount_i8r7
-- 
-- -- calls charCounter_i8r7 on the unparsed board and the given side
-- 
-- Arguments:
     -- side: current side
     -- board: parsed board
-- Returns:
     -- number tiles of current side in board
tileCount_i8r7 :: Char -> [String] -> Int
tileCount_i8r7 side board = 
    charCounter_i8r7 (unparseBoard_i8r7 board) side

-- charCounter_i8r7
-- 
-- -- counts the number of a given side that are in the board string
-- 
-- Arguments:
     -- board: unparsed board
     -- side: current side
-- Returns:
     -- number tiles of current side in board
charCounter_i8r7 :: String -> Char ->  Int 
charCounter_i8r7 board side
    | null board                = 0
    | (head board) == side      = 1 + (charCounter_i8r7 (tail board) side)
    | otherwise                 = charCounter_i8r7 (tail board) side

-- isWin_i8r7
-- 
-- -- is this board a win for original side?
-- 
-- Arguments:
     -- side: current side
     -- origSide: original side given
     -- board: parsed board
     -- boardHistory: list of string
     -- boardSize: size of board
-- Returns:
     -- true if winning board
isWin_i8r7 :: Char -> Char -> [String]  -> [String] -> Int -> Bool
isWin_i8r7 side origSide board boardHistory boardSize 
    | (cannotMove_i8r7 side board)&&(side /= origSide)  = True
    | tooFewTiles_i8r7 (changeSide_i8r7 side) board boardSize       = True
    | otherwise                                                     = False


-- not continue from a losing heuristic board. !!!
-- (if you cannot move, you'll only have the original board)
--
-- newCoordinates_i8r7
-- 
-- -- will find the coordinates that we can legally move to
-- 
-- Arguments:
     -- x: position in line
     -- y: line in board
     -- origBoard: parsed board
     -- side: current side
     -- factor: 1 if slide 2 if hop
-- Returns:
     -- list of legal move coordinates
newCoordinates_i8r7 :: Int -> Int -> [String] -> Char -> Int -> [(Int,Int)]
newCoordinates_i8r7 x y origBoard side factor = 
    filter (isLegal_i8r7 (x,y) origBoard side factor) (findCoords_i8r7 x y (length (head origBoard)) factor)

-- isLegal_i8r7
-- 
-- -- checks if move (slide of hop) is legal
-- 
-- Arguments:
     -- (x,y): original tile coordinate 
     -- origBoard: parsed board
     -- side: current side
     -- factor: 1 if slide 2 if hop
     -- (a,b): new move coordinate
-- Returns:
     -- true if legal move
isLegal_i8r7 :: (Int,Int) -> [String] -> Char -> Int -> (Int,Int) -> Bool
isLegal_i8r7 (x,y) origBoard side factor (a,b)
    | factor == 1       = isSlideLegal_i8r7 (a,b) origBoard
    | factor == 2       = isHopLegal_i8r7 (x,y) (a,b) origBoard side

-- isSlideLegal_i8r7
-- 
-- -- checks if tile can slide to (a,b)
-- 
-- Arguments:
     -- (a,b): new move coordinate
     -- origBoard: parsed board
-- Returns:
     -- true if legal slide move
isSlideLegal_i8r7 :: (Int,Int) -> [String] -> Bool
isSlideLegal_i8r7 (a,b) origBoard
    | not(outOfBounds_i8r7 (a,b) origBoard)     = 
        ((findTile_i8r7 (a,b) origBoard) == '-')
    | otherwise                                 = False

-- isHopLegal_i8r7
-- 
-- -- checks if tile can hop to (a,b)
-- 
-- Arguments:
     -- (x,y): original tile coordinate
     -- (a,b): new move coordinate
     -- origBoard: original side given
     -- side: current side
-- Returns:
     -- true if hop is legal move
isHopLegal_i8r7 :: (Int,Int) -> (Int,Int) -> [String] -> Char -> Bool
isHopLegal_i8r7 (x,y) (a,b) origBoard side
    | not(outOfBounds_i8r7 (a,b) origBoard)     = (landingOK_i8r7 (a,b) origBoard side) &&
        (hopOverOK_i8r7 (x,y) (a,b) origBoard side)
    | otherwise                                 = False

-- landingOK_i8r7
-- 
-- -- check if hop is landing on a '-' or an opposite side tile
-- 
-- Arguments:
     -- (a,b): new move coordinate
     -- origBoard: parsed board
     -- side: current side
-- Returns:
     -- true if can land
landingOK_i8r7 :: (Int,Int) -> [String] -> Char -> Bool
landingOK_i8r7 (a,b) origBoard side = 
        (((findTile_i8r7 (a,b) origBoard) == '-') || 
        ((findTile_i8r7 (a,b) origBoard) == (changeSide_i8r7 side)))

-- hopOverOK_i8r7
-- 
-- -- calls isIntersectTileSide_i8r7
-- 
-- Arguments:
     -- (x,y): original tile coordinate
     -- (a,b): new move coordinate
     -- origBoard: parsed board
     -- side: current side
-- Returns:
     -- true if can hop over tile
hopOverOK_i8r7 :: (Int,Int) -> (Int,Int) -> [String] -> Char -> Bool
hopOverOK_i8r7 (x,y) (a,b) origBoard side = 
    isIntersectTileSide_i8r7 (findCoords_i8r7 x y (length (head origBoard)) 1) 
            (findCoords_i8r7 a b (length (head origBoard)) 1) 
            origBoard side 

-- isIntersectTileSide_i8r7
-- 
-- -- is the coord that is the intersect of these two lists == (x,y)'s side
-- 
-- Arguments:
     -- xySlides: list of legal slides from (x,y)
     -- abSlides: list of legal slides form (a,b)
     -- origBoard: parsed board
     -- origSide: original side given
-- Returns:
--      true if the intersect is a tile
isIntersectTileSide_i8r7 :: [(Int,Int)] -> [(Int,Int)] -> [String] -> Char -> Bool
isIntersectTileSide_i8r7 xySlides abSlides origBoard origSide = 
    isIntersectCorrectSide_i8r7 (getIntersectCoord_i8r7 xySlides abSlides) origBoard origSide

-- isIntersectCorrectSide_i8r7
-- 
-- -- checks to see if the intersecting coordinate of both lists has the correct side
-- 
-- Arguments:
     -- intersect: tile intersection
     -- origBoard: parsed board
     -- origSide: original side given
-- Returns:
--      true if intersect is equal to original side
isIntersectCorrectSide_i8r7 :: [(Int,Int)] -> [String] -> Char -> Bool
isIntersectCorrectSide_i8r7 intersect origBoard origSide
    | null intersect            = False
    | otherwise                 = (findTile_i8r7 (head intersect) origBoard) == origSide

-- getIntersectCoord_i8r7
-- 
-- -- get the coord that is in both lists
-- 
-- Arguments:
     -- xySlides: list of legal slides from (x,y)
     -- abSlides: list of legal slides form (a,b)
-- Returns:
--      the intersect tile of the two lists
getIntersectCoord_i8r7 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
getIntersectCoord_i8r7 xySlides abSlides = 
    (filter (isCoordInList_i8r7 abSlides) xySlides)

-- isCoordInList_i8r7
-- 
-- -- checks if coord is in list
-- 
-- Arguments:
     -- abSlides: list of legal slides form (a,b)
     -- coord: coordinate from (x,y) slides
-- Returns:
--      true if coord is in (a,b) slides
isCoordInList_i8r7 :: [(Int,Int)] -> (Int,Int) -> Bool
isCoordInList_i8r7 abSlides coord
    | null abSlides                 = False
    | coord == (head abSlides)      = True
    | otherwise                     = False || (isCoordInList_i8r7 (tail abSlides) coord)

-- findTile_i8r7
-- 
-- -- finds and returns tile at coord
-- 
-- Arguments:
     -- (a,b): a coordinate
     -- origBoard: parsed board
-- Returns:
     -- return the tile at coordinate given
findTile_i8r7 :: (Int,Int) -> [String] -> Char
findTile_i8r7 (a,b) origBoard
    | b == 0        = findTileInLine_i8r7 (head origBoard) a 
    | otherwise     = findTile_i8r7 (a, (b - 1)) (tail origBoard)

-- findTileInLine_i8r7
-- 
-- -- finds and returns tile at x position on line
-- 
-- Arguments:
     -- line: substring of board 
     -- a: the position in line iterator
-- Returns:
     -- return tile at position a
findTileInLine_i8r7 :: String -> Int -> Char
findTileInLine_i8r7 line a 
    | a == 0        = (head line)
    | otherwise     = findTileInLine_i8r7 (tail line) (a - 1)

-- outOfBounds_i8r7
-- 
-- -- finds if coord is out of bounds of board
-- 
-- Arguments:
     -- (a,b): a coordinate
     -- origBoard: parsed board
-- Returns:
--      true if coordinate is outside board
outOfBounds_i8r7 :: (Int,Int) -> [String] -> Bool
outOfBounds_i8r7 (a,b) origBoard 
    | null origBoard    = True
    | b < 0             = True
    | b == 0            = xCoordOutOfBounds_i8r7 a (head origBoard)
    | otherwise         = outOfBounds_i8r7 (a, (b - 1)) (tail origBoard)

-- xCoordOutOfBounds_i8r7
-- 
-- -- finds if the x is out of bounds
-- 
-- Arguments:
     -- a: position in line iterator
     -- line: substring of board 
-- Returns:
     -- true if position a is in line
xCoordOutOfBounds_i8r7 :: Int -> String -> Bool
xCoordOutOfBounds_i8r7 a line = 
    not ( ((-1) < a) && (a < (length line)) )

-- findCoords_i8r7
-- 
-- -- calls adjustedCoordMaker_i8r7 on each case of line expansion
-- 
-- Arguments:
     -- x: the original tile position in line
     -- y: the original tile line in board
     -- n: board size
     -- factor: 1 if slide 2 if hop
-- Returns:
--      list of possible coordinates
findCoords_i8r7 :: Int -> Int -> Int  -> Int -> [(Int,Int)]
findCoords_i8r7 x y n factor
    | y < (n - 2)   = adjustedCoordsMaker_i8r7 x y xAdjustSlideUp xAdjustHopUpUp factor
    | y == (n - 2)  = adjustedCoordsMaker_i8r7 x y xAdjustSlideUp xAdjustHopUpMd factor
    | y == (n - 1)  = adjustedCoordsMaker_i8r7 x y xAdjustSlideMd xAdjustHopMd factor
    | y == n        = adjustedCoordsMaker_i8r7 x y xAdjustSlideDn xAdjustHopDnMd factor
    | y > n         = adjustedCoordsMaker_i8r7 x y xAdjustSlideDn xAdjustHopDnDn factor
    where
        xAdjustSlideUp = [-1,0] ++ [-1,1] ++ [0, 1]     
        xAdjustSlideMd = [-1,0] ++ [-1,1] ++ [-1,0]
        xAdjustSlideDn = [0, 1] ++ [-1,1] ++ [-1,0]
--          move           up        md        dn
        xAdjustHopUpUp   = [-2,0] ++ [-2,2] ++ [0, 2]
        xAdjustHopUpMd   = [-2,0] ++ [-2,2] ++ [1,-1]
        xAdjustHopMd     = [-2,0] ++ [-2,2] ++ [0,-2]
        xAdjustHopDnMd   = [-1,1] ++ [-2,2] ++ [0,-2]
        xAdjustHopDnDn   = [ 0,2] ++ [-2,2] ++ [0,-2]

-- adjustedCoordsMaker_i8r7
-- 
-- -- calls adjustedSlideCoords_i8r7 or adjustedHopCoords_i8r7
-- 
-- Arguments:
     -- x: the original tile position in line
     -- y: the original tile line in board
     -- xAdjustSlide: x adjustment for a slide
     -- xAdjustHop: x adjustment for a hop
     -- factor: 1 if slide 2 if hop
-- Returns:
     -- list of possible coordinates
adjustedCoordsMaker_i8r7 :: Int -> Int -> [Int] -> [Int] -> Int -> [(Int,Int)]
adjustedCoordsMaker_i8r7 x y xAdjustSlide xAdjustHop factor
    | factor == 1               = adjustedSlideCoords_i8r7 x y xAdjustSlide yAdjustSlide
    | factor == 2               = adjustedHopCoords_i8r7 x y xAdjustHop yAdjustHop
    where
        yAdjustSlide = [-1,-1] ++ [0,0] ++ [1,1]
        yAdjustHop   = [-2,-2] ++ [0,0] ++ [2,2]
--          moves        up        md        dn

-- adjustedSlideCoords_i8r7
-- 
-- -- zips together the adjusted Xs and Ys into new slide coords
-- 
-- Arguments:
     -- x: the original tile position in line
     -- y: the original tile line in board
     -- xAdjustSlide: x adjustment for a slide
     -- yAdjustSlide: y adjustment for a slide
-- Returns:
     -- list of possible slide coordinates
adjustedSlideCoords_i8r7 :: Int -> Int -> [Int] -> [Int]-> [(Int,Int)]
adjustedSlideCoords_i8r7 x y xAdjustSlide yAdjustSlide = 
    zip adjustedXs adjustedYs
    where 
        adjustedYs = (map (+ y) yAdjustSlide)
        adjustedXs = (map (+ x) xAdjustSlide) 

-- adjustedHopCoords_i8r7
-- 
-- -- zips together the adjusted Xs and Ys into new hop coords
-- 
-- Arguments:
     -- x: the original tile position in line
     -- y: the original tile line in board
     -- xAdjustHop: x adjustment for a hop
     -- yAdjustHop: y adjustment for a hop
-- Returns:
     -- list of possible hop coordinates
adjustedHopCoords_i8r7 :: Int -> Int -> [Int] -> [Int]-> [(Int,Int)]
adjustedHopCoords_i8r7 x y xAdjustHop yAdjustHop = 
    zip adjustedXs adjustedYs
    where 
        adjustedYs = (map (+ y) yAdjustHop)
        adjustedXs = (map (+ x) xAdjustHop) 











-- searchTheTree_i8r7
-- 
-- -- calls selectBestNextMove_i8r7 with the root's children
-- 
-- Arguments:
     -- tree:
-- Returns:
     -- 
searchTheTree_i8r7 :: [([String],Int,[[String]])] -> [String]
searchTheTree_i8r7 tree = 
    selectBestNextMove_i8r7 tree (getRootChildren_i8r7 tree) 1 (getDepth_i8r7 (getRoot_i8r7 tree))

-- selectBestNextMove_i8r7
-- 
-- -- finds max score of accumulated scores of each of the root's children
-- 
-- Arguments:
     -- tree: list of nodes
     -- rootChildren: list of child boards of the root
     -- depth: current depth
     -- rootDepth: depth of the root
-- Returns:
--      the board that's the best next move
selectBestNextMove_i8r7 :: [([String],Int,[[String]])] -> [[String]] -> Int -> Int -> [String]
selectBestNextMove_i8r7 tree rootChildren depth rootDepth = 
    fst (getExtrema_i8r7 (minMaxRootChildren_i8r7 tree (getNodes_i8r7 tree rootChildren rootDepth) 1) 1)

-- minMaxRootChildren_i8r7
-- 
-- -- applies minMax_i8r7 to all the root's children
-- 
-- Arguments:
     -- tree: list of nodes
     -- nextMoves: list of nodes of the root's children
     -- depth: current depth
-- Returns:
--      list of scores for each of the root's children
minMaxRootChildren_i8r7 :: [([String],Int,[[String]])] -> [([String],Int,[[String]])] -> Int -> [([String],Int)]
minMaxRootChildren_i8r7 tree nextMoves depth = 
    map (minMax_i8r7 tree depth) nextMoves

-- minMax_i8r7
-- 
-- -- recurse down the tree and accumulate score on your way up
-- 
-- Arguments:
     -- tree: list of nodes
     -- depth: current depth
     -- startNode: root of the subtree
-- Returns:
     -- score of the root of the subtree
minMax_i8r7 :: [([String],Int,[[String]])] -> Int -> ([String],Int,[[String]]) -> ([String],Int)
minMax_i8r7 tree depth startNode 
    | not(hasChildren_i8r7 startNode)       = ((getBoard_i8r7 startNode), (getHeuristic_i8r7 startNode))
    | otherwise                             = 
        sumTuppleScores_i8r7
            ((getBoard_i8r7 startNode), 
                (getHeuristic_i8r7 startNode)) 
            (getExtrema_i8r7 
                (map (minMax_i8r7 tree (depth + 1) ) (getChildNodes_i8r7 tree startNode))
                depth)

-- sumTuppleScores_i8r7
-- 
-- -- adds the extrema score of the children to the current tupple score 
-- 
-- Arguments:
     -- startBoard: parsed board
     -- startScore: accumulated score of start board
     -- extremeBoard: a parsed board
     -- extremeScore: accumulated score of extreme board
-- Returns:
     -- new score with the start boad and the new accumulated score
sumTuppleScores_i8r7 :: ([String],Int) -> ([String],Int) -> ([String],Int)
sumTuppleScores_i8r7 (startBoard, startScore) (extremeBoard, extremeScore) = 
    (startBoard, (startScore + extremeScore))

-- hasChildren_i8r7
-- 
-- -- checks if a node has children
-- 
-- Arguments:
     -- node: node in the tree
-- Returns:
--      true if node has children
hasChildren_i8r7 :: ([String],Int,[[String]]) -> Bool
hasChildren_i8r7 node = 
    not (null (getChildren_i8r7 node))

-- getBoard_i8r7
-- 
-- -- return board of node
-- 
-- Arguments:
     -- (board,_,_): node in the tree
-- Returns:
--      board
getBoard_i8r7 :: ([String],Int,[[String]]) -> [String]
getBoard_i8r7 (board,_,_) = 
    board


-- getHeuristic_i8r7
-- 
-- --returns the heuristic of node
-- 
-- Arguments:
     -- (_,heuristic,_): node in the tree
-- Returns:
--      heuristic
getHeuristic_i8r7 :: ([String],Int,[[String]]) -> Int
getHeuristic_i8r7 (_,heuristic,_) = 
    heuristic


-- getExtrema_i8r7
-- 
-- -- calls minScore or maxScore depending on odd or even
-- 
-- Arguments:
     -- scores: list of score
     -- depth: current depth
-- Returns:
--      the extreme score based on depth
getExtrema_i8r7 :: [([String],Int)] -> Int -> ([String],Int)
getExtrema_i8r7 scores depth
    | (isEven_i8r7 depth)       = minScore_i8r7 scores (head scores)
    | otherwise                 = maxScore_i8r7 scores (head scores)

-- isEven_i8r7
-- 
-- -- is number even
-- 
-- Arguments:
     -- num: an Int
-- Returns:
     -- true if even
isEven_i8r7 :: Int -> Bool
isEven_i8r7 num =
    (mod num 2) == 0


-- minScore_i8r7
-- 
-- -- finds the tupple with the smallest score
-- 
-- Arguments:
    -- scores: list of score
    -- bestScore: current min score
-- Returns:
--      the minimum score
minScore_i8r7 :: [([String],Int)] -> ([String],Int) -> ([String],Int)
minScore_i8r7 scores bestScore
    | null scores                               = bestScore
    | ((snd (head scores)) < (snd bestScore))   = minScore_i8r7 (tail scores) (head scores)
    | otherwise                                 = minScore_i8r7 (tail scores) bestScore

-- maxScore_i8r7
-- 
-- -- finds the tupple with the largest score
-- 
-- Arguments:
     -- scores: list of score
     -- bestScore: current max score
-- Returns:
--      the maximum score
maxScore_i8r7 :: [([String],Int)] -> ([String],Int) -> ([String],Int)
maxScore_i8r7 scores bestScore
    | null scores                               = bestScore
    | ((snd (head scores)) > (snd bestScore))   = maxScore_i8r7 (tail scores) (head scores)
    | otherwise                                 = maxScore_i8r7 (tail scores) bestScore

-- getRootChildren_i8r7
-- 
-- -- returns the list of the root's children's boards
-- 
-- Arguments:
     -- tree: list of nodes
-- Returns:
     -- the list of the root's children's boards
getRootChildren_i8r7 :: [([String],Int,[[String]])] -> [[String]]
getRootChildren_i8r7 tree
    | not(hasParents_i8r7 (head tree) tree)     = getChildren_i8r7 (head tree)
    | otherwise                                 = getRootChildren_i8r7 (tail tree)

-- getRoot_i8r7
-- 
-- -- returns the root node
-- 
-- Arguments:
     -- tree: list of nodes
-- Returns:
--      the root node
getRoot_i8r7 :: [([String],Int,[[String]])] -> ([String],Int,[[String]])
getRoot_i8r7 tree
    | not(hasParents_i8r7 (head tree) tree)     = (head tree)
    | otherwise                                 = getRoot_i8r7 (tail tree)

-- hasParents_i8r7
-- 
-- -- checks if a child node has a perent in the tree
-- 
-- Arguments:
     -- child: node to check from
     -- tree: list of nodes
-- Returns:
     -- true if node has parent
hasParents_i8r7 :: ([String],Int,[[String]]) -> [([String],Int,[[String]])] -> Bool
hasParents_i8r7 child tree 
    | null tree                             = False
    | (isYourChild_i8r7 child (head tree))  = True
    | otherwise                             = False || (hasParents_i8r7 child (tail tree))

-- isYourChild_i8r7
-- 
-- -- checks if first node parameter is child of second node parameter
-- 
-- Arguments:
     -- (childBoard,h,loc): node of tree
     -- (parentBoard,parentHeuristic,children): node of tree
-- Returns:
--      true if child node is child of parent node
isYourChild_i8r7 :: ([String],Int,[[String]]) -> ([String],Int,[[String]]) -> Bool
isYourChild_i8r7 (childBoard,h,loc) (parentBoard,parentHeuristic,children) = 
    (isAmongChildren_i8r7 childBoard children)&&(checkChildDepth_i8r7 (childBoard,h,loc) (getDepth_i8r7 (parentBoard,parentHeuristic,children)))

-- isAmongChildren_i8r7
-- 
-- -- is board in the list of another node's children
-- 
-- Arguments:
     -- childBoard: parsed board
     -- children: list of parsed board
-- Returns:
--      true if childBoard is in children
isAmongChildren_i8r7 :: [String] -> [[String]] -> Bool
isAmongChildren_i8r7 childBoard children
    | null children                     = False 
    | childBoard == (head children)     = True
    | otherwise                         = False || (isAmongChildren_i8r7 childBoard (tail children))

-- getChildren_i8r7
-- 
-- -- get the list of a node's children from the node given
-- 
-- Arguments:
     -- (_,_,children): node in tree
-- Returns:
--      children
getChildren_i8r7 :: ([String],Int,[[String]]) -> [[String]]
getChildren_i8r7 (_,_,children) = 
    children

-- getChildNodes_i8r7
-- 
-- -- get nodes of the children of the node
-- 
-- Arguments:
     -- tree: list of nodes
     -- node: node in tree
-- Returns:
     -- nodes of children of node
getChildNodes_i8r7 :: [([String],Int,[[String]])] -> ([String],Int,[[String]]) -> [([String],Int,[[String]])]
getChildNodes_i8r7 tree node = 
    getNodes_i8r7 tree (getChildren_i8r7 node) (getDepth_i8r7 node)

-- getNodes_i8r7
-- 
-- -- given a list of boards, find their respective nodes in the tree
-- 
-- Arguments:
     -- tree: list of nodes
     -- boards: parsed board list
     -- parentDepth: parent depth
-- Returns:
--      list of node of boards given
getNodes_i8r7 :: [([String],Int,[[String]])] -> [[String]] -> Int -> [([String],Int,[[String]])]
getNodes_i8r7 tree boards parentDepth
    | null boards       = []
    | otherwise         = (getNode_i8r7 tree (head boards) parentDepth) : (getNodes_i8r7 tree (tail boards) parentDepth)

-- getNode_i8r7
-- 
-- -- given a board, find its node in the tree
-- 
-- Arguments:
     -- tree: list of nodes
     -- board: a parsed board
     -- parentDepth: parent depth
-- Returns:
--      node of board
getNode_i8r7 :: [([String],Int,[[String]])] -> [String] -> Int -> ([String],Int,[[String]])
getNode_i8r7 tree board parentDepth
    | (isYourChildNode_i8r7 (head tree) board parentDepth)      = (head tree)
    | otherwise                                                     = getNode_i8r7 (tail tree) board parentDepth

-- isYourChildNode_i8r7
-- 
-- -- checks if given node is parent's child using board comparison and child depth
-- 
-- Arguments:
     -- treeNode: node of tree
     -- board: parsed board
     -- parentDepth: parent depth
-- Returns:
--      true if tree node's board is board and has proper depth
isYourChildNode_i8r7 :: ([String],Int,[[String]]) -> [String] -> Int -> Bool
isYourChildNode_i8r7 treeNode board parentDepth = 
    ((getBoard_i8r7 treeNode) == board)&&(checkChildDepth_i8r7 treeNode parentDepth)

-- checkChildDepth_i8r7
-- 
-- -- cheks if node is an appropriate depth to be the parent's child
-- 
-- Arguments:
     -- treeNode: node of tree
     -- parentDepth: parent depth
-- Returns:
--      true if tree node if appropriate depth
checkChildDepth_i8r7 :: ([String],Int,[[String]]) -> Int -> Bool
checkChildDepth_i8r7 treeNode parentDepth = 
    (getDepth_i8r7 treeNode) == (parentDepth + 1)

-- getDepth_i8r7
-- 
-- -- retrieves depth from node heuristic
-- 
-- Arguments:
     -- (_,h,_): node
-- Returns:
--      node depth
getDepth_i8r7 :: ([String],Int,[[String]]) -> Int
getDepth_i8r7 (_,h,_) = 
    (h - ((div h depthBuffer_i8r7) * depthBuffer_i8r7)) 
    
-- addDepth_i8r7
-- 
-- -- adds depth to node heuristic
-- 
-- Arguments:
     -- origHeuristic: heuristic
     -- depth: current depth
-- Returns:
     -- depth moded heuristic
addDepth_i8r7 :: Int -> Int -> Int 
addDepth_i8r7 origHeuristic depth = 
    (origHeuristic * depthBuffer_i8r7) + depth

-- depthBuffer_i8r7
-- 
-- -- the buffer used to find the depth from the heuristic
-- 
-- Returns:
     -- 100000
depthBuffer_i8r7 :: Int
depthBuffer_i8r7 = 100000