


module Practice2 where
import Data.Char

----------------------------------------------------------------------
--A phone book
---------------------------------------------------------------------

type Name = String
type PhoneNumber = Int
type Person  = (Name, PhoneNumber)
type PhoneBook = [Person]


-- Part a)
add :: Person -> PhoneBook -> PhoneBook
add (n,pn) pb = (n,pn) : pb




-- Part b)
delete  :: Name -> PhoneBook -> PhoneBook
delete n l = filter(\(x,y) -> x/=n) l

--  Part c)
find  :: Name -> PhoneBook -> [PhoneNumber]
find n l = snd(unzip(filter(\(x,y) -> x==n) l))

--  Part d)
update :: Name ->  PhoneNumber ->  PhoneNumber->   PhoneBook ->PhoneBook
update n opn npn l= (filter(\(x,y) -> y /= opn && x/=n) l) ++ [(n,npn)]


-----------------------------------------------------------------
-- Customers of a Bank
-----------------------------------------------------------------

type NI = Int
type Age = Int
type Balance = Float
type Customer  = (NI,Age, Balance)
type Bank =  [Customer]

-- Part a)
retired :: Customer -> Bool
retired (insn,age,b) | age>=60 = True
                     | otherwise = False

-- Part b)
deposit :: Customer -> Float -> Customer
deposit (insn,age,b) toAdd = (insn,age,b+toAdd)


-- Part c)
withdraw :: Customer -> Float -> Customer
withdraw (insn,age,b) toSub | b-toSub >= 0 = (insn,age,b-toSub)
                            |otherwise = (insn,age,b)

-- Part d)
credit :: Bank -> [Customer]
credit l = filter(\(insn,age,b) -> b>= 0) l


-----------------------------------------------------------------
-- 
-----------------------------------------------------------------

cubeOdds  :: [Int]-> [Int]
cubeOdds xs = [x^3 | x <- xs, x `mod` 2 == 1]

cubeOdds2 :: [Int]-> [Int]
cubeOdds2 l = map(^3)(filter(\(x) -> x`mod` 2 ==1) l)


-----------------------------------------------------------------
-- 
-----------------------------------------------------------------

repChar :: (Char, Char) -> String -> String
repChar (x,y) l = map(\a -> if a==x then y else a)l


-----------------------------------------------------------------
-- 
-----------------------------------------------------------------

--repStr :: (String, String) -> String -> String
repStr (fs,ss) "" = ""
repStr (fs,ss) (x:y:xs)
               |y==head(fs) && x/= head(fs) = [x] ++ repStr (fs,ss) (y:xs)
               |y == head(fs) && x==head(fs) = ss ++ repStr (fs,ss) xs
               |(y/=head (fs) && x/= head(fs) ) || (y /=head(fs) && x==head(fs)) = [x]++[y]++repStr(fs,ss) xs
               |otherwise = [x]


-----------------------------------------------------------------
-- 
-----------------------------------------------------------------

-- Part a)

zap :: [Int] -> [Int] -> [(Int,Int)]
zap [] [] = []
zap xs [] = []
zap [] ys = []
zap (x:xs) (y:ys) = (x,y) :zap xs ys

-- Part b)
addIndex :: [Int] -> [(Int,Int)] 
addIndex l = zip [1..length l] l
-- Part c)
extend :: Int -> String -> String
extend k s |length s <= k = extend k (s++"_")  
           |k < 0 = "Negative Input"
           |k >=0 = take k s
                    
           
           



