

module Practice4 where



---------------------------------------------------------------------
----- 
---------------------------------------------------------------------
data Value = A|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|J|Q|K
             deriving (Eq, Ord, Enum)
instance Show Value
--- Part a)
 where show  A ="A"
       show Two ="2"
       show Three ="3"
       show Four ="4"
       show Five ="5"
       show Six ="6"
       show Seven ="7"
       show Eight ="8"
       show Nine ="9"
       show Ten ="10"
       show J ="J"
       show Q ="Q"
       show K ="K"
       
data Suite = Hearts | Spades | Clubs | Diamonds
             deriving (Eq, Ord, Enum)
instance Show Suite
--- Part b)
 where show Hearts = "H"
       show Spades = "S"
       show Clubs = "C"
       show Diamonds = "D"

data Colour = Red | Black
              deriving (Eq, Ord,Enum, Show)

data Error a = Fail|Ok a
               deriving (Eq, Ord, Show)



type Card  = (Value, Suite)


--- Part c)

pack :: [Card]
pack = [(value,suite) | value <- [A .. K] ,suite <- [Hearts .. Diamonds]]


--- Part d)

--colour :: Card -> Colour
colour (value,suite) | ((suite == Clubs) || (suite == Spades)) = Black
                     | otherwise = Red


--- Part e)

split :: Int -> [a] -> (Error ([a],[a]))
split x list |(length list > x && x >= 0 ) = Ok (take x list , drop x list)
             |otherwise = Fail



interleave ::  [a] ->  [a] -> [a]
interleave (x:xs) (y:ys) = x : y : interleave xs ys 
interleave [] [] = []
interleave x [] = x
interleave [] y = y



--- Part f)

shuffle :: [Int] -> [a] -> Error [a]
shuffle [] list = Ok list
shuffle (x:xs) list | (length list > x && x >= 0)  = shuffle (xs) (interleave(shuffleLeft (split x list))(shuffleRight (split x list)))
                    | otherwise= Fail 
shuffleLeft :: Error([a],[b]) -> [a]
shuffleLeft (Ok (a,b)) = a

shuffleRight :: Error([a],[b]) -> [b]
shuffleRight (Ok (a,b)) = b




---------------------------------------------------------------------
----- 
---------------------------------------------------------------------


data Btree a = ND | Data a |  Branch (Btree a) (Btree a)
               deriving (Show,Eq)

data Dir = L | R 
           deriving (Show,Eq)

type Path =  [Dir] 
    
--- Part a)

extract :: Path  -> Btree a -> Error a
extract (L:xs) (Branch bTree1 bTree2) = extract xs bTree1
extract (R:xs) (Branch bTree1 bTree2) = extract xs bTree2
extract [] (Data x) = Ok x
extract _ _ = Fail


--- Part b)

add :: a -> Path -> Btree a -> Error (Btree a)
add n [] (ND) = Ok (Data n)
add n [] _ = Fail
add n [L] (Data m) = Fail
add n [R] (Data m) = Fail
add n (R:xs) (ND) = case (add n xs (ND)) of
                    Ok x -> Ok(Branch (ND) x)
                    Fail -> Fail
add n (R:xs) (Branch bTree1 bTree2) = case (add n xs bTree2) of
                                      Ok x -> Ok (Branch (bTree1) x)
                                      Fail -> Fail
add n (L:xs) (ND) = case (add n xs ND) of
                    Ok x -> Ok (Branch x (ND))
                    Fail -> Fail
add n (L:xs) (Branch bTree1 bTree2) = case (add n xs bTree1) of
                                      Ok x -> Ok (Branch x (bTree2))
                                      Fail -> Fail




tree1 = Branch ND ND
tree2 = Branch ND (Data 3)
tree3 = Branch tree1 tree2
tree4 = Branch (Data 3) (Data 4)
tree5 = Branch tree3 tree4



---------------------------------------------------------------------
----- Family tree
---------------------------------------------------------------------


-- c
sort:: Ord a => (a -> a -> Bool) -> [a] -> [a]
sort a [] = []
sort a [b] = [b]
sort a (x:y:xs) | (a x y) == False = put a y (sort a (x:xs))
                | otherwise = put a x (sort a (y:xs))
put :: Ord a =>(a->a->Bool) -> a -> [a] -> [a]
put a x [] = [x]
put a x (y:ys) | (a x y) == True = x:y:ys 
               |otherwise = y:(put a x ys)

-- d
data Tree a = U | F a (Tree a) (Tree a) deriving Show

term = F "Anna" (F "Fer-Jan" (F "Willem" U U) (F "Nettie" U U)) (F "Paula" (F "Mario" U U) (F "Martha" U U))
type Person = String 

genlabel :: (Tree Person) -> (Tree (Int,Person))
genlabel U = U
genlabel (F x l r ) = getCount 1 (F x l r)

getCount :: Int -> (Tree Person) -> (Tree (Int,Person))
getCount _ U = U
getCount 1 (F x l r) = F (1,x) (getCount 2 l) (getCount 3 r)
getCount counter (F x l r) = F (counter,x) (getCount (2*counter) l) (getCount ((2*counter)+1) r)


--e
preprint ::  (Int,Person) -> String
preprint (x,n) = (show x) ++ " : " ++ n



type Tile = [String]

flatten :: Tile -> String
flatten [] = []
flatten (y:ys) = y ++ "\n" ++ flatten ys 





printlist :: Tree Person -> IO()
printlist U = putStr ""
printlist (F x l r) = putStr (flatten (sort (<) (treelist(genlabel term))))


treelist :: (Tree (Int,Person)) -> Tile 
treelist U = []
treelist (F x l r) = [(preprint x)] ++ (treelist l) ++ (treelist r)




tree = F "Anna" (F "Fer-Jan" (F "Willem" U U) (F "Nettie" U U)) (F "Paula" (F "Mario" U U) (F "Martha" U U))


test1 = printlist tree

--f

print2Dtree :: Tree Person -> IO()
print2Dtree U = putStr ""
print2Dtree (F x l r) = putStr (flatten (toPrint 0 (F x l r)))


toPrint :: Int ->Tree Person -> [String]
toPrint _ U = []
toPrint n (F x l r) = (toPrint(n+1) l) ++ [(spacing n) ++ x] ++ (toPrint (n+1) r)


spacing :: Int -> String
spacing 0 = " "
spacing counter = "\t" ++ spacing (counter -1)


test2 = print2Dtree tree




