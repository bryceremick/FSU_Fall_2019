import Data.List

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' ::(Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

-- max' :: (Ord a) => [a] -> a
max' [] = error "must contain elements"
max' [x] = x
max' (x:xs) 
    | x > acc = x
    | otherwise = acc
    where acc = max' xs


replicate' :: (Num i, Ord i) => i -> a -> [a] 
replicate' n x 
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x 

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x:take' (n-1) xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool 
elem' _ [] = False
elem' n (x:xs) 
    | n == x = True
    | otherwise = elem' n xs


-- quicksort :: (Ord a) => [a] -> [a]  
-- quicksort [] = []  
-- quicksort (x:xs) =   
--     let smallerSorted = quicksort [a | a <- xs, a <= x]  
--         biggerSorted = quicksort [a | a <- xs, a > x]  
--     in  smallerSorted ++ [x] ++ biggerSorted 

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted where
    smallerSorted = quicksort [a | a <-xs, a <= x]
    biggerSorted = quicksort [a | a <- xs, a > x]

fact :: (Eq a, Num a) => a -> a
fact 0 = 1
fact n = n * fact (n-1)

-- cat :: [a] -> [a] -> [a]
-- test xs [] = xs
-- test [] ys = ys
-- test (x:xs) (y:ys) = xs:y

data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

dot :: LOL a -> LOL a -> LOL a
dot (LOL n1 xs) (LOL n2 ys) = LOL (n1 + n2) (xs++ys)

l1 = LOL 4 "poop"
l2 = LOL 3 "ack"

-- test :: LOL a -> [a]
test (LOL n xs) = n