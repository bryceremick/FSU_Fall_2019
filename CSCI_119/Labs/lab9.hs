import Data.List

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
conv r = undefined


-- Test, and show your tests! You may copy code from previous labs to help.


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

-}