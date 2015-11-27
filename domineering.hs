import System.IO
-- Definition of moves
type Move = (Int,Int)

horMoves :: Int -> Int -> [Move]
horMoves m n = [(x,y) | x <- [0..((n*m)-1)], y <- [0..((n*m)-1)], x /= y, x - y == 1, ((x `div` m) == (y `div` m))]
             
verMoves :: Int -> Int -> [Move]
verMoves m n = [(x,y) | x <- [0..((n*m)-1)], y <- [0..((n*m)-1)], x /= y, x - y == m]
             
modify :: [Move] -> Int -> [Move]
modify [] _ = []
modify ((a,b):xs) n = if(a == n || b == n) then modify xs n
                       else (a,b):(modify xs n)
-- Definition of situations

type Situation = [[Bool]]
initialSituation :: Int -> Int -> Situation
initialSituation m n = [ ([False | x <- [0..(m-1)]]) | y <- [0..(n-1)]]

-- Definition of players

data Player = H | V deriving (Eq,Show)

-- Definition of outcomes

type Outcome = Int

-- Definition of board

data Board = Board {
                 columns      :: Int
               , rows         :: Int
               , player       :: Player      -- next player
               , hmoves       :: [Move]      -- X moves so far (sorted)
               , vmoves       :: [Move]      -- O moves so far (sorted)
               , hPossibleM   :: [Move]      -- H Possible moves
               , vPossibleM   :: [Move]      -- V Possible Moves
               , outcome      :: Outcome     -- optimal outcome for this board
               --, situation    :: Situation   -- situation as a list of booleans
              }
              
initialBoard :: Int -> Int -> Board
initialBoard m n= Board m n H [] [] (horMoves m n) (verMoves m n) 0 --(initialSituation n m)

update :: Situation -> Move -> (Bool,Situation)
update situation (m,n) = undefined

winning :: Situation -> Bool
winning []          = True
winning (x:xs)      = (checkAllTrue x) && (winning xs) where
checkAllTrue []     = True
checkAllTrue (x:xs) = if(x==True) then checkAllTrue xs
                      else False
-- Playing functions

play :: Move -> Board -> Board
play (m,n) (Board c r H xs os hPoss vPoss outcome) = let xs' = insert (m,n) xs in
  let hPoss' = (modify (modify hPoss m) n) in
  let vPoss' = (modify (modify vPoss m) n) in
  if (vPoss' == [])
     then Board c r V xs' os hPoss' vPoss'            1
     else Board c r V xs' os hPoss' vPoss'            (if((length vPoss') > (length hPoss')) then (-1) else 1)
play (m,n) (Board c r V xs os hPoss vPoss outcome) = let os' = insert (m,n) os in
  let hPoss' = (modify (modify hPoss m) n) in
  let vPoss' = (modify (modify vPoss m) n) in
  if (hPoss' == [])
     then Board c r H xs os' hPoss' vPoss'           (-1)
     else Board c r H xs os' hPoss' vPoss'           (if((length vPoss') > (length hPoss')) then (-1) else 1)
play m board = error "Bug when using function play."


winningBoard :: Board -> Bool
winningBoard board = if((player board == V)&&(vPossibleM board == []))then True
                     else if((player board == H)&&(hPossibleM board == [])) then True else False

--Trees

data Tree = Fork {root :: Board, children :: [(Move,(Tree,Int,Int))]}

{-|treeOf :: Board -> Tree
treeOf board | null(hPossibleM board) || null (vPossibleM board) = Fork board [] 
             | otherwise                = Fork board' forest
  where
    forest = if(player board == H) then [(m, treeOf'(play m board)) | m <- (hPossibleM board)]
                                   else [(m, treeOf'(play m board)) | m <- (vPossibleM board)]   
    board'
     | player board == H = board {outcome = maximum [outcome(root t) | (_,t)<-forest]}
     | otherwise         = board {outcome = minimum [outcome(root t) | (_,t)<-forest]} -}
treeOf' :: Board -> Int -> Int -> Tree
treeOf' board n l | winningBoard board || null(hPossibleM board) || null (vPossibleM board) = Fork board [] 
                  | otherwise                                                   = Fork board' forest
  where
    forest = if(n > l) then []
               else
               if(player board == H) then [(m, ((treeOf'(play m board) (n+1) l),n,l)) | m <- (hPossibleM board)]
                                     else [(m, ((treeOf'(play m board) (n+1) l),n,l)) | m <- (vPossibleM board)]    
    board'
     | player board == H = board {outcome = supremum [outcome(root t) | (_,(t,_,_))<-forest]}
     | otherwise         = board {outcome = infimum  [outcome(root t) | (_,(t,_,_))<-forest]}
upgradeTree :: Tree -> Int -> Int -> Tree
upgradeTree (Fork board children) n l = Fork board' forest
    where
    forest = if(n > l) then []
               else
               if(player board == H) then [(m, ((treeOf'(play m board) (n+1) l),n,l)) | m <- (hPossibleM board)]
                                     else [(m, ((treeOf'(play m board) (n+1) l),n,l)) | m <- (vPossibleM board)]    
    board'
     | player board == H = board {outcome = supremum [outcome(root t) | (_,(t,_,_))<-forest]}
     | otherwise         = board {outcome = infimum  [outcome(root t) | (_,(t,_,_))<-forest]}
tictactoe :: Int -> Int -> Int -> Tree
tictactoe m n l = treeOf' (initialBoard m n) 0 l

noplays :: Tree -> Int 
noplays (Fork _ []) = 1
noplays (Fork _ forest) = sum [noplays tree | (_,(tree,_,_)) <- forest]


optimalMoves :: Tree -> [(Move,Tree)]
optimalMoves (Fork board forest) = if player board == H then [(m,t) | (m,(t,_,_))<-forest, outcome(root t) == outcome board]
                                                        else [(m,t) | (m,(t,_,_))<-forest, outcome(root t) == outcome board]

elementary :: Int -> [Move] -> Bool
elementary n [] = False
elementary n ((a,b):xs) = if(n==a || n==b) then True
                           else elementary n xs
instance Show Board where
  show (Board m n pl xs os hpos vpos oc) =
       show pl ++ " plays next\n"
    ++ "The optimal outcome is " ++ show oc ++ "\n\n" 
    ++ (getString m n (numLength (n*m) 0)) 
    where
      numLength p acc = if (p == 0) then acc
                                    else numLength (p `div` 10) (acc+1)
      f m grade | elementary m xs = show H ++ (if(m<10) then "" else (numSpaces ((getNum grade m))))
                | elementary m os = show V ++ (if(m<10) then "" else (numSpaces ((getNum grade m))))
                | otherwise   = show m
      getString m n grade = toString [0..(n*m -1)] m 0 grade
      getNum n m = if(m==0) then (n - (numLength 1 0))
                            else (n - (numLength m 0))
      toString [] m count grade = ""
      toString (x:xs) m count grade = if((count `div` (m-1) == 0)) then (f x grade) ++ (numSpaces (getNum grade x)) ++ "|" 
                                                                        ++ "  " ++(toString xs m (count + 1) grade)
                                                                   else (f x grade) ++ "\n\n" ++ (toString xs m 0 grade)
      numSpaces n = if (n==(-1)) then "" else " " ++ (numSpaces (n-1))




usersTurn :: Tree -> IO()
usersTurn (Fork board []) = do
  putStrLn(show board)
  putStrLn("Game over with outcome " ++ show(outcome board))
usersTurn (tree@(Fork board forest)) = do
  putStrLn(show board)
  --utStrLn("The list of outcomes from now on is " ++ show(outcomes' tree))
  putStrLn("Please play ")
  putStr("Enter first coordinate: ")
  hFlush stdout
  column <- getLine
  let columnM = read column
  putStr("Enter second coordinate: ")
  hFlush stdout
  row <- getLine
  let rowM = read row
  case lookup (columnM,rowM) forest of -----------------DO TURN AROUND
               Nothing -> case lookup (rowM,columnM) forest of Nothing -> do
                                                                          putStrLn "Invalid move. Try again."
                                                                          usersTurn (Fork board forest)
                                                               Just (tree,_,l) -> do
                                                                            if winningBoard board
                                                                            then putStrLn (show board ++ "\nYou win.")
                                                                            else computersTurn (upgradeTree tree 0 l)   
               Just (tree,_,l) -> do
                            if winningBoard board
                            then putStrLn (show board ++ "\nYou win.")
                            else computersTurn (upgradeTree tree 0 l)                                                                                                     
                       
choose :: Int -> [a] -> a
choose n [x] = x
choose n (x:xs) = if n == 0 then x else choose (n-1) xs

computersTurn :: Tree -> IO()
computersTurn (Fork board []) = do
  putStrLn(show board)
  putStrLn("Game over with outcome " ++ show(outcome board))
computersTurn (tree@(Fork board forest)) = do
  putStrLn(show board)
  --putStrLn("The list of outcomes from now on is " ++ show(outcomes' tree))
  putStrLn("I am thinking...")
  putStrLn("The optimal outcome is " ++ show(outcome board))
  let myMoves = optimalMoves tree
  putStrLn("My optimal moves are " ++ show [ m | (m,_) <- myMoves])
  if null myMoves
     then putStrLn "I lost."
     else do
       let (m,subtree) = choose 0 myMoves     
       putStrLn("I play " ++ show m)
       let Fork board' forest' = subtree
       case lookup m forest of Nothing -> error "Trying to make a move that is not in the tree"
                               Just (tree,_,l) -> if winningBoard board' 
                                                  then putStrLn(show board' ++ "\nI win.")
                                                  else if null forest'
                                                       then putStrLn("\nGame over.")
                                                       else usersTurn (upgradeTree tree 0 l)                       


     
-- LIMIT THESE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
supremum :: [Outcome] -> Outcome
supremum []        = -1 
supremum (1:xs)    =  1
supremum ((-1):xs) = supremum xs
supremum (0:xs)    = supremum0 xs
  where
    supremum0 []     = 0
    supremum0 (1:xs) = 1
    supremum0 (_:xs) = supremum0 xs

infimum :: [Outcome] -> Outcome
infimum []        =  1 
infimum (1:xs)    = infimum xs
infimum ((-1):xs) = -1
infimum (0:xs)    = infimum0 xs
  where
    infimum0 []        = 0
    infimum0 ((-1):xs) = -1
    infimum0 (_:xs)    = infimum0 xs
    

outcomes :: Tree -> [Outcome]
outcomes (Fork board []) = [outcome board]
outcomes (Fork _ trees) = concat [outcomes tree | (_,(tree,_,_)) <- trees]

type Set a = a -> Bool

singleton :: Eq a => a -> Set a
singleton x = \y -> x == y

union :: [Set a] -> Set a
union ps = \x -> or [p x | p <- ps]

outcomesSet :: Tree -> Set Outcome
outcomesSet (Fork board []) = singleton(outcome board)
outcomesSet (Fork _ forest) = union[outcomesSet tree | (_,(tree,_,_)) <- forest]


outcomes' :: Tree -> [Outcome]
outcomes' tree = [x | x<-[-1,1], outcomesSet tree x]
{-|
play :: Move -> Board -> Board
play (m,n) (Board c r H xs os outcome situation) = if ((m-n) == (-1) || (m-n) == 1) then
   let correct = update situation (m,n) in
   case correct of (False,_) -> undefined
                   (True, updatedSituation) ->
                                           if(winning updatedSituation) 
                                           then Board c r V (insert (m,n) xs) os 1 updatedSituation
                                           else Board c r V (insert (m,n) xs) os 0 updatedSituation
   else error "Not a valid move"
play (m,n) (Board c r V xs os outcome situation) = if ((m-n) == c || (m-n) == (-c)) then
   let correct = update situation (m,n) in
   case correct of (False,_) -> undefined
                   (True, updatedSituation) ->
                                           if(winning updatedSituation) 
                                           then Board c r H (insert (m,n) xs) os 1 updatedSituation
                                           else Board c r H (insert (m,n) xs) os 0 updatedSituation
   else error "Not a valid move"
play m board = error "Bug when using function play." -}
















contained :: Ord x => [x] -> [x] -> Bool
contained [] ys = True
contained xs [] = False
contained (us@(x : xs)) (y : ys) 
    | x == y    = contained xs ys
    | x >= y    = contained us ys
    | otherwise = False

someContained :: Ord x => [[x]] -> [x] -> Bool
someContained [] ys = False
someContained xss [] = False
someContained (xs : xss) ys = contained xs ys || someContained xss ys

insert :: Ord x => x -> [x] -> [x]
insert x [] = [x]
insert x (vs@(y : ys)) 
    | x == y       = vs
    | x <  y       = x : vs
    | otherwise    = y : insert x ys

delete :: Ord x => x -> [x] -> [x]
delete x [] = []
delete x (vs@(y : ys))
    | x == y    = ys 
    | x <  y    = vs
    | otherwise = y : delete x ys 
