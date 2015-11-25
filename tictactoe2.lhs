Playing TicTacToe optimally using game trees
--------------------------------------------

This is a variation of tictactoe.lhs, in which we replace the meaning
of the field "outcome" by the optimal outcome (rather than the current
outcome) in the construction of game trees. This makes the program
both shorter and faster. We also change some other functions to take
advantage of this.

> import System.IO

There is plenty of room for making things more efficient.  We favour
clarity here. But we do care a little bit about efficiency. For
example, we work with sorted lists of moves, and we implement a form
of alpha-beta prunning.

The board and moves are

             0 | 1 | 2
            ---+---+---
             3 | 4 | 5
            ---+---+---
             6 | 7 | 8

> type Move = Int -- We will use only 0..8.

> possibleMoves :: [Move]
> possibleMoves = [0..8]

A list of moves wins if it contains a line, a column or a diagonal.

> wins :: [Move] -> Bool
> wins = someContained [[0,1,2],[3,4,5],[6,7,8], -- lines
>                       [0,3,6],[1,4,7],[2,5,8], -- columns
>                       [0,4,8],[2,4,6]]         -- diagonals

The players are

> data Player = X | O deriving (Eq,Show)

The outcome of the game can be 

    1  = X wins
    0  = draw
   -1  = O wins

Player X tries to maximize the outcome, and 
player O tries to minimize it.

> type Outcome = Int -- We will use only -1, 0, 1.

A board can be represented by a pair (xs,os) where xs is the list of X
moves and os is the list of O moves. 

   * When the length of the list xs++os is even, it is X's turn to
     play.

   * However, in order to make things more efficient, we will store
     whose turn it is.

   * The list of available moves consists of all moves in possibleMoves
     which are not in xs++os.

   * But, again for the sake of efficiency, we will keep the list of
     allowed moves in the board representation.

   * We will also store the optimal outcome of the game for
     efficiency, even though it can again be computed from the other data.

> data Board = Board {
>                 player       :: Player   -- next player
>               , xmoves       :: [Move]   -- X moves so far (sorted)
>               , omoves       :: [Move]   -- O moves so far (sorted)
>               , allowedMoves :: [Move]   -- allowed moves in this board
>               , outcome      :: Outcome  -- optimal outcome for this board
>              }

> initialBoard :: Board
> initialBoard = Board X [] [] possibleMoves 0

To play a move, we don't check whether is valid or the board is
valid. We instead assume that whenever the funcion play is called, it
is called with an allowed move. Crucially, the function play updates
the allowed moves, in particular to be empty when a player wins. This
is a potential source of bugs, and so should be used carefully.

> play :: Move -> Board -> Board
> play m (Board X xs os as 0) = let xs' = insert m xs in
>   if wins xs' 
>      then Board O xs' os []            1
>      else Board O xs' os (delete m as) 0
> play m (Board O xs os as 0) = let os' = insert m os in
>   if wins os'
>      then Board X xs os' []          (-1)
>      else Board X xs os' (delete m as) 0
> play m board = error "Bug when using function play."

The function winning needs to be different from that it tictactoe.lhs:

> winning :: Board -> Bool
> winning board = outcome board /= 0 && allowedMoves board == []

The trees are as before, but we define them with record syntax:

> data Tree = Fork {root :: Board, children :: [(Move,Tree)]}

We refer to an element of the type [(Move,Tree)] as a forest.  A leaf
is a fork with an empty forest.

Given a board, we build the game tree that starts with that board, but
instead of storing the current outcomes as in tictactoe.lhs, we
compute and store the optimal outcomes, using the minimax algorithm
during the construction of the tree:

> treeOf :: Board -> Tree
> treeOf board | null(allowedMoves board) = Fork board [] 
>              | otherwise                = Fork board' forest
>   where
>     forest = [(m, treeOf(play m board)) | m <- allowedMoves board]  
>     board'
>      | player board == X = board {outcome = maximum [outcome(root t) | (_,t)<-forest]}
>      | otherwise         = board {outcome = minimum [outcome(root t) | (_,t)<-forest]}

We also have treeOf' below, which uses a form of alpha-beta prunning.

> tictactoe :: Tree
> tictactoe = treeOf' initialBoard

The number of plays is the number of paths, which is the same as the number of leaves:

> noplays :: Tree -> Int 
> noplays (Fork _ []) = 1
> noplays (Fork _ forest) = sum [noplays tree | (_,tree) <- forest]

*Main> noplays tictactoe 
255168

This means that there are 255168 valid plays (which is less than
9!=362880) . This is 2^6 * 3^2 * 443, where 443 is a prime
number. However, some of these 443 plays end up in the same board
position.

Assuming that both players play rationally, the following gives the
optimal outcome of a game (this is the minimax algorithm without
alpha-beta pruning -- another function optimalOutcome' which uses a
form of alpha-beta prunning is defined below.).

> optimalMoves :: Tree -> [(Move,Tree)]
> optimalMoves (Fork board forest) = [(m,t) | (m,t)<-forest, outcome(root t) == outcome board]

We now play interactively by navigating the tree. We show the board as
follows:

> instance Show Board where
>   show (Board pl xs os as oc) =
>        show pl ++ " plays next\n"
>     ++ "The optimal outcome is " ++ show oc ++ "\n\n" 
>     ++ f 0 ++ " | " ++ f 1 ++ " | " ++ f 2 ++ "\n"
>     ++ "--+---+--\n"
>     ++ f 3 ++ " | " ++ f 4 ++ " | " ++ f 5 ++ "\n"
>     ++ "--+---+--\n"
>     ++ f 6 ++ " | " ++ f 7 ++ " | " ++ f 8 ++ "\n"
>     where
>       f m | m `elem` xs = show X
>           | m `elem` os = show O
>           | otherwise   = show m

We call this function when it is the user's turn to play:

> usersTurn :: Tree -> IO()
> usersTurn (Fork board []) = 
>   putStrLn("Game over with outcome " ++ show(outcome board))
> usersTurn (tree@(Fork board forest)) = do
>   putStrLn(show board)
>   putStrLn("The list of outcomes from now on is " ++ show(outcomes' tree))
>   putStr("Please play: ")
>   hFlush stdout
>   s <- getLine
>   let m = read s
>   case lookup m forest of
>     Nothing -> do
>       putStrLn "Invalid move. Try again."
>       usersTurn (Fork board forest)
>     Just tree -> do
>       if winning board
>          then putStrLn (show board ++ "\nYou win.")
>          else computersTurn tree

We use this function to experiment with computersTurn below (n=4 seems
to be the worst choice).

> choose :: Int -> [a] -> a
> choose n [x] = x
> choose n (x:xs) = if n == 0 then x else choose (n-1) xs

We call this function when it is the computer's turn to play:

> computersTurn :: Tree -> IO()
> computersTurn (Fork board []) = 
>   putStrLn("Game over with outcome " ++ show(outcome board))
> computersTurn (tree@(Fork board forest)) = do
>   putStrLn(show board)
>   putStrLn("The list of outcomes from now on is " ++ show(outcomes' tree))
>   putStrLn("I am thinking...")
>   putStrLn("The optimal outcome is " ++ show(outcome board))
>   let myMoves = optimalMoves tree
>   putStrLn("My optimal moves are " ++ show [ m | (m,_) <- myMoves])
>   if null myMoves
>      then putStrLn "Draw."
>      else do
>        let (m,subtree) = choose 0 myMoves 
>        putStrLn("I play " ++ show m)
>        let Fork board' forest' = subtree
>        if winning board' 
>           then putStrLn(show board' ++ "\nI win.")
>           else if null forest'
>                   then putStrLn("Draw")
>                   else usersTurn subtree

Now choose who starts:

> main :: IO()
> main = computersTurn tictactoe

That's it.

Appendix. This gives a form of alpha-beta prunning, by simply changing
the minimum and maximum functions with cleverer ones which avoid
consuming their whole input if they can.

> treeOf' :: Board -> Tree
> treeOf' board | winning board || null(allowedMoves board) = Fork board [] 
>               | otherwise                                 = Fork board' forest
>   where
>     forest = [(m, treeOf'(play m board)) | m <- allowedMoves board]  
>     board'
>      | player board == X = board {outcome = supremum [outcome(root t) | (_,t)<-forest]}
>      | otherwise         = board {outcome = infimum  [outcome(root t) | (_,t)<-forest]}

This is about 5 times faster than the original version when applied to
tictactoe, with the following definitions:

> supremum :: [Outcome] -> Outcome
> supremum []        = -1 
> supremum (1:xs)    =  1
> supremum ((-1):xs) = supremum xs
> supremum (0:xs)    = supremum0 xs
>   where
>     supremum0 []     = 0
>     supremum0 (1:xs) = 1
>     supremum0 (_:xs) = supremum0 xs

> infimum :: [Outcome] -> Outcome
> infimum []        =  1 
> infimum (1:xs)    = infimum xs
> infimum ((-1):xs) = -1
> infimum (0:xs)    = infimum0 xs
>   where
>     infimum0 []        = 0
>     infimum0 ((-1):xs) = -1
>     infimum0 (_:xs)    = infimum0 xs

Appendix. The list of all optimal outcomes of a tree (its leaves):

> outcomes :: Tree -> [Outcome]
> outcomes (Fork board []) = [outcome board]
> outcomes (Fork _ trees) = concat [outcomes tree | (_,tree) <- trees]

We use functions Outcome -> Bool to represent (decidable) subsets of
the type Outcome. Only non-empty subsets will arise.

> type Set a = a -> Bool

> singleton :: Eq a => a -> Set a
> singleton x = \y -> x == y

> union :: [Set a] -> Set a
> union ps = \x -> or [p x | p <- ps]

> outcomesSet :: Tree -> Set Outcome
> outcomesSet (Fork board []) = singleton(outcome board)
> outcomesSet (Fork _ forest) = union[outcomesSet tree | (_,tree) <- forest]

Then, with the following definition, the following definition sorts
and removes duplicates from outcomes, without doing that explicitly,
and is much faster (thanks to both short cut evaluation || used to
define or and to lazy evaluation).

> outcomes' :: Tree -> [Outcome]
> outcomes' tree = [x | x<-[-1,0,1], outcomesSet tree x]

Using this, we may consider an improvement of optimalMoves. If the
oponnent is not rational or occasionally makes mistakes, we should
choose moves which lead to subtrees in which it is possible to
win. TODO.

If we want to use probabilities to play better against a player that
makes mistakes, perhaps we can use functions Outcome -> Int to
represent multisets of the type Outcome.

> type MSet a = a -> Int

> msingleton :: Eq a => a -> MSet a
> msingleton x = \y -> if x == y then 1 else 0

> munion :: [MSet a] -> MSet a
> munion ps = \x -> sum [p x | p <- ps]

> outcomesMSet :: Tree -> MSet Outcome
> outcomesMSet (Fork board []) = msingleton(outcome board)
> outcomesMSet (Fork _ forest) = munion[outcomesMSet tree | (_,tree) <- forest]

This is slow, but gives useful information:

> outcomes'' :: Tree -> [Int]
> outcomes'' tree = [outcomesMSet tree x | x <- [-1,0,1]]

*Main> outcomes'' tictactoe
[77904,46080,131184]

This means that of all possible plays, 77904 are won by X, 46080 are
draws, and 131184 are won by O. This is respectively 31%, 18%, 51%.

Appendix. Our list library, for the sake of self-containedness,
follows. We work with ordered lists without repetitions, for
efficiency.

> contained :: Ord x => [x] -> [x] -> Bool
> contained [] ys = True
> contained xs [] = False
> contained (us@(x : xs)) (y : ys) 
>     | x == y    = contained xs ys
>     | x >= y    = contained us ys
>     | otherwise = False

> someContained :: Ord x => [[x]] -> [x] -> Bool
> someContained [] ys = False
> someContained xss [] = False
> someContained (xs : xss) ys = contained xs ys || someContained xss ys

> insert :: Ord x => x -> [x] -> [x]
> insert x [] = [x]
> insert x (vs@(y : ys)) 
>     | x == y       = vs
>     | x <  y       = x : vs
>     | otherwise    = y : insert x ys

> delete :: Ord x => x -> [x] -> [x]
> delete x [] = []
> delete x (vs@(y : ys))
>     | x == y    = ys 
>     | x <  y    = vs
>     | otherwise = y : delete x ys 
