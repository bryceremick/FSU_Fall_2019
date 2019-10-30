import Data.List
import Data.Array
import Control.Monad (replicateM)  -- for strings function below

data RegExp = Empty
             | Let Char
             | Union RegExp RegExp
             | Cat RegExp RegExp
             | Star RegExp
             deriving (Show, Eq)

data RegExp' = Zero | One | Let' Char |
               Union' [RegExp'] | Cat' [RegExp'] | Star' RegExp'
  deriving (Eq, Ord, Show)

-- Finite state machines (as records), indexed by the type of their states
type FSM a = ([a], a, [a], a -> Char -> a)

-- Convert ordinary RegExps into extended REs
fromRE :: RegExp -> RegExp'
fromRE Empty = Zero
fromRE (Let c) = Let' c
fromRE (Union r1 r2) = Union' [fromRE r1, fromRE r2]
fromRE (Cat r1 r2) = Cat' [fromRE r1, fromRE r2]
-- fromRE (Star Empty) = One
fromRE (Star r1) = Star' (fromRE r1)

-- Convert extended REs into ordinary REs, eliminating Union' and Cat' on
-- lists of length < 2, and right-associating them on longer lists
fromRE' :: RegExp' -> RegExp
fromRE' Zero = Empty
fromRE' One = Star Empty
fromRE' (Let' c) = Let c
fromRE' (Union' []) = Empty
fromRE' (Union' [r]) = fromRE' r
fromRE' (Union' (r:rs)) = Union (fromRE' r) (fromRE' (Union' rs))
fromRE' (Cat' []) = Star Empty
fromRE' (Cat' [r]) = fromRE' r
fromRE' (Cat' (r:rs)) = Cat (fromRE' r) (fromRE' (Cat' rs))
fromRE' (Star' r) = Star (fromRE' r)

-- Basic postfix parser for RegExp', assuming binary + and ., for testing
toRE' :: String -> RegExp'
toRE' w = go w [] where
  go [] [r] = r
  go ('0':xs) rs = go xs (Zero:rs)
  go ('1':xs) rs = go xs (One:rs)
  go ('+':xs) (r2:r1:rs) = go xs (Union' [r1,r2]:rs)
  go ('.':xs) (r2:r1:rs) = go xs (Cat' [r1,r2]:rs)
  go ('*':xs) (r:rs) = go xs (Star' r:rs)
  go (x:xs) rs = go xs (Let' x:rs)

toRE :: String -> RegExp
toRE w = go w [] where
  go [] [r]              = r
  go ('+':xs) (r2:r1:rs) = go xs (Union r1 r2:rs)
  go ('.':xs) (r2:r1:rs) = go xs (Cat r1 r2:rs)
  go ('*':xs) (r:rs)     = go xs (Star r:rs)
  go ('@':xs) rs         = go xs (Empty:rs)
  go (x:xs) rs           = go xs (Let x:rs)

simp :: RegExp' -> RegExp'
simp Zero = Zero
simp One = One
simp (Let' c) = Let' c
simp (Union' rs) = union' $ flat_uni $ map simp rs
simp (Cat' rs) = union' $ flat_cat $ map simp rs
simp (Star' r) = star' $ simp r


-- Smart constructor for Union' that normalizes its arguments and
-- handles the empty and singleton cases
union' :: [RegExp'] -> RegExp'
union' rs =  case norm rs of
  []  ->  Zero
  [r] -> r
  rs  -> Union' rs

-- Smart constructor for Cat' that handles the empty and singleton cases
cat' :: [RegExp'] -> RegExp'
cat' [] = One
cat' [r] = r
cat' rs = Cat' rs

-- Smart constructor for Star' that handles the constant and Star' cases
star' :: RegExp' -> RegExp'
star' Zero = One
star' One = One
star' (Star' r) = star' r
star' r = Star' r

-- Flatten a list of arguments to Union'; assumes each argument is simplified
flat_uni :: [RegExp'] -> [RegExp']
flat_uni [] = []
flat_uni (Zero:rs) = flat_uni rs
flat_uni (Union' rs':rs) = rs' ++ flat_uni rs
flat_uni (r:rs) = r : flat_uni rs

-- Flatten a list of arguments to Cat', turning them into a list of arguments
-- to Union'; assumes each argument is simplified
flat_cat :: [RegExp'] -> [RegExp']
flat_cat rs = fc [] rs where
  -- fc (args already processed, in reverse order) (args still to be processed)
  fc :: [RegExp'] -> [RegExp'] -> [RegExp']
  fc pr [] = [cat' $ reverse pr]
  fc pr (Zero:rs) = []
  fc pr (One:rs) = fc pr rs
  fc pr (Cat' rs':rs) = fc (reverse rs' ++ pr) rs
  fc pr (Union' rs':rs) = concat $ map (fc pr . (:rs)) rs'
  fc pr (r:rs) = fc (r:pr) rs

norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
rad [] = []
rad [x] = [x]
rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys
  

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr

-- reachable m == the part of m that is reachable from the start state
reachable :: Ord a => FSM a -> FSM a
reachable m@(qs, s, fs, d) = (qs', s, fs', d) where
  qs' = uclosure [s] (\q -> map (d q) sigma)
  fs' = filter (`elem` fs) qs'

-- Change the states of an FSM from an equality type to Int 
-- and use an array lookup for the transition function
intify :: Eq a => FSM a -> FSM Int
intify (qs, s, fs, d) = ([0..n-1], s', fs', d') where
  n = length qs
  m = length sigma
  s'  = ind qs s
  fs' = map (ind qs) fs
  arr = listArray ((0,0), (n-1,m-1)) [ind qs (d q a) | q <- qs, a <- sigma]
  d' q a = arr ! (q, ind sigma a)
  ind (q':qs) q = if q == q' then 0 else 1 + ind qs q

reduce :: Ord a => FSM a -> FSM Int
reduce = intify . reachable

splits :: [a] -> [([a], [a])]
splits xs = [(take x xs, drop x xs) | x <- [0..(length xs)]]

accept2_aux :: Eq a => FSM a -> a -> [Char] -> Bool
accept2_aux m@(_, _, fs, _) q [] = elem q fs
accept2_aux m@(_, _, _, d) q (a:w) = accept2_aux m (d q a) w

accept2 :: Eq a => FSM a -> [Char] -> Bool
accept2 m@(_, s, _, _) w = accept2_aux m s w

match1 :: RegExp -> String -> Bool
match1 Empty w = False
match1 (Let c) "" = False
match1 (Let c) (w:ws) = c == w && ws == []
match1 (Union r1 r2) w = (match1 r1 w) || (match1 r2 w)
match1 (Cat r1 r2) w = or [ (match1 r1 w1) && (match1 r2 w2) | (w1, w2) <- (splits w) ]
match1 (Star r1) w = w == "" || or [(match1 r1 w1) && (match1 (Star r1) w2) | (w1, w2) <- (splits w), w1 /= ""]

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]
                   


---------- LAB 9 BEGINS HERE --------------

-- Given alphabet. But your code should work with any finite list of characters.
sigma = ['a', 'b']

-- Bypassability for extended regular expressions, computed directly by recursion.
byp :: RegExp' -> Bool
byp Zero = False
byp One = True
byp (Let' _) = False
byp (Union' rs) = or [byp r| r <- rs] 
byp (Cat' rs) = and [byp r| r <- rs] 
byp (Star' _) = True

-- -- Regular-expression derivatives (aka left quotients) on extended regular
-- -- expressions, computed directly by recursion.
deriv :: Char -> RegExp' -> RegExp'
deriv s Zero = Zero
deriv s One = Zero
deriv s (Let' c)
    | s == c = One
    | otherwise = Zero
deriv s (Union' rs) = (Union' [deriv s r | r <- rs])
deriv s (Cat' (r:rs))
    | byp r = (Union' ([Cat' ([deriv s r]++rs)]++[deriv s r | r <- rs]))
    | otherwise = (Cat' ([deriv s r]++rs))
deriv s (Star' r) = (Cat' [deriv s r, Star' r])

-- Convert an RegExp' to an FSM using the derivative (Brzozowski) construction.
-- States are SIMPLIFIED extended REs.  Note: to construct all the states,
-- you will have to use a unary closure process.
conv :: RegExp' -> FSM RegExp'
conv r = (qs, s, fs, d) where
  qs = uclosure [simp r] (\q -> [simp (deriv a q) | a <- sigma])
  s = simp r
  fs = [q | q <- qs, byp q]
  d q a = simp (deriv a q)


---------- LAB 9 ENDS HERE --------------


-- TESTING 

-- Test, and show your tests! You may copy code from previous labs to help.

testRegExps :: [RegExp']
testRegExps = [toRE' "a", toRE' "ab.", toRE' "ab+", toRE' "a*", toRE' "ab.ba..ab.+", toRE' "ab+ba.+ab.."]

test = and [ accept2 (conv r) s == match1 (fromRE' r) s | r <- testRegExps, s <- strings 5]

{-

--! a \ a
*Main> deriv 'a' (toRE' "a")
One

--! a \ ab
*Main> deriv 'a' (toRE' "ab.")
Cat' [One,Let' 'b']
*Main> simp it
Let' 'b'

--! a \ ba
*Main> deriv 'a' (toRE' "ba.")
Cat' [Zero,Let' 'a']
*Main> simp it
Zero

--! a \ (a + b)
*Main> deriv 'a' (toRE' "ab+")
Union' [One,Zero]
*Main> simp it
One

--! a \ (ab + ba)
*Main> deriv 'a' (toRE' "ab.ba.+")
Union' [Cat' [One,Let' 'b'],Cat' [Zero,Let' 'a']]
*Main> simp it
Let' 'b'

--! a \ (ab + ba)*
*Main> deriv 'a' (toRE' "ab.ba.+*")
Cat' [Union' [Cat' [One,Let' 'b'],Cat' [Zero,Let' 'a']],Star' (Union' [Cat' [Let' 'a',Let' 'b'],Cat' [Let' 'b',Let' 'a']])]
*Main> simp it
Cat' [Let' 'b',Star' (Union' [Cat' [Let' 'a',Let' 'b'],Cat' [Let' 'b',Let' 'a']])]



--! Test a list of RegExp's by comparing the results of accept2 and match1

-- testRegExps = [toRE' "a", toRE' "ab.", toRE' "ab+", toRE' "a*", toRE' "ab.ba..ab.+", toRE' "ab+ba.+ab.."]
-- test = and [ accept2 (conv r) s == match1 (fromRE' r) s | r <- testRegExps, s <- strings 5]

-- test
-- True
-}