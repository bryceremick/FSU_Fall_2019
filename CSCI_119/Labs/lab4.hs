-- CSci 119, Lab 4

import Data.List (sort, stripPrefix, nub) -- for your solution to Lab 3
import Control.Monad (replicateM)    -- for strings function at the end
import Data.Maybe


-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Length-Ordered Lists over "character type" a (aka "strings")
-- Invariant: In LOL n xs, n == length xs
data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is sorted with no duplicates
type Lang a = [LOL a]

-- Smart constructor for (finite) languages
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

---- Regular expressions, along with input and output
data RegExp = Empty                -- Empty language
            | Let Char             -- Single letter language
            | Union RegExp RegExp  -- Union
            | Cat RegExp RegExp    -- Concatenation
            | Star RegExp          -- Kleene star
            deriving (Show, Eq)

-- Compact display form for RegExp
newtype Compact = Compact RegExp

instance (Show Compact) where    -- use precedence to minimize parentheses
  showsPrec d (Compact r) = sp d r where
    sp d Empty         = showString "@"
    sp d (Let c)       = showString [c]
    sp d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                         sp 6 r1 .
                         showString "+" .
                         sp 6 r2
    sp d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                         sp 7 r1 .
                         sp 7 r2
    sp d (Star r1)     = sp 9 r1 .     -- prec(Star) = 8
                         showString "*"

-- Quick and dirty postfix RegExp parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = go w [] where
  go [] [r]              = r
  go ('+':xs) (r2:r1:rs) = go xs (Union r1 r2:rs)
  go ('.':xs) (r2:r1:rs) = go xs (Cat r1 r2:rs)
  go ('*':xs) (r:rs)     = go xs (Star r:rs)
  go ('@':xs) rs         = go xs (Empty:rs)
  go (x:xs) rs           = go xs (Let x:rs)


---------------- Your solution to Lab 3 ----------------

-- Include part of your solution to Lab 3 here for testing purposes.
-- After a few days, I will release my solution for you to use. Don't
-- duplicate the code just given above.

cart :: [a] -> [b] -> [(a,b)]
cart xs ys = [(a,b) | a <- xs, b <- ys]

powerh :: [a] -> [[a]]
powerh [] = [[]]
powerh (x:xs) = [] : map (x:) (powerh xs) ++ powerh xs
power xs = nub $ powerh xs -- ¯\_(ツ)_/¯

dot :: LOL a -> LOL a -> LOL a
dot (LOL n1 xs) (LOL n2 ys) = LOL (n1 + n2) (xs++ys)

rev :: LOL a -> LOL a
rev (LOL n xs) = LOL n (reverse xs)

merge :: Ord a => Lang a -> Lang a -> Lang a
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xr) ys@(y:yr) =
  case compare x y of
    LT -> x : merge xr ys
    EQ -> x : merge xr yr
    GT -> y : merge xs yr

cat :: Ord a => Lang a -> Lang a -> Lang a
cat [] _ = []
cat _ [] = []
cat (x:xr) ys@(y:yr) = dot x y : merge (map (dot x) yr) (cat xr ys)

kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr 
kstar xs = eps : cat xs (kstar xs)

stripPrefixLOL (LOL n1 xs) (LOL n2 ys) = stripPrefix xs ys

leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq x [] = []
leftq x (y:ys)
    | isJust z = (lol $ fromJust z):leftq x ys
    | otherwise = leftq x ys
    where 
      z = stripPrefixLOL x y

lang_of :: RegExp -> Lang Char
lang_of Empty = []
lang_of (Let c) = lang [[c]]
lang_of (Cat r1 r2) = (lang_of r1) `cat` (lang_of r2)
lang_of (Union r1 r2) = (lang_of r1) `merge` (lang_of r2)
lang_of (Star r1) = kstar (lang_of r1)

onestr :: String -> RegExp
onestr [] = Star Empty
onestr [x] = Let x
onestr (x:xs) = Cat (Let x) (onestr xs)

finite :: [String] -> RegExp
finite [] = Empty
finite [w] = onestr w
finite (w:ws) = Union (onestr w) (finite ws)


lang1 = lang ["aa", "ab", "aaa", "abc"]
lang2 = lang ["b", "aa", "ab", "aa"]

---------------- Part 1 ----------------

-- Membership for languages that satisfy the invariant (sorted, no duplicates),
-- even if they are infinite. Only look at the contents of a string when it is
-- necessary to do so, and stop as soon as you know the answer.
-- Compare lengths first, THEN compare actual strings
memb :: Ord a => LOL a -> Lang a -> Bool
memb _ [] = False
memb (LOL n1 x1) ((LOL n2 x2):xs)
  | (n1 < n2) = False 
  | (x1 == x2) = True
  | otherwise = memb (LOL n1 x1) xs 


-- Implement the seven recursive predications/operations on RegExp given in
-- Section 3.3 of the notes; call them empty, unitary, byp, inf, rever, lq,
-- and nep. Each should begin with a type declaration. Include several tests
-- for each function.

empty :: RegExp -> Bool
empty Empty = True
empty (Let _) = False
empty (Union r1 r2) = empty r1 && empty r2
empty (Cat r1 r2) = empty r1 || empty r2
empty (Star _) = False

unitary :: RegExp -> Bool
unitary Empty = False
unitary (Let _) = False
unitary (Union r1 r2) = (unitary r1 && empty r2) || (empty r1 && unitary r2) || (unitary r1 && unitary r2)
unitary (Cat r1 r2) = unitary r1 && unitary r2
unitary (Star r1) = empty r1 || unitary r1

byp :: RegExp -> Bool
byp Empty = False
byp (Let _) = False
byp (Union r1 r2) = byp r1 || byp r2
byp (Cat r1 r2) = byp r1 && byp r2
byp (Star _) = True

inf :: RegExp -> Bool
inf Empty = False
inf (Let _) = False
inf (Union r1 r2) = inf r1 || inf r2
inf (Cat r1 r2) = (inf r1 && (not (empty r2))) || (inf r2 && (not (empty r1)))
inf (Star r1) = (not (empty r1)) && (not (unitary r1))

-- ----------------------

rever :: RegExp -> RegExp
rever Empty = Empty
rever (Let c) = Let c
rever (Union r1 r2) = (Union (rever r1) (rever r2))
rever (Cat r1 r2) = (Cat (rever r2) (rever r1))
rever (Star r1) = (Star (rever r1))

lq :: Char -> RegExp -> RegExp
lq s Empty = Empty
lq s (Let c)
  | s == c = (Star (Empty))
  | otherwise = Empty
lq s (Union r1 r2) = (Union (lq s r1) (lq s r2))
lq s (Cat r1 r2)
  | byp r1 = (Union (Cat (lq s r1) (r2)) (lq s r2))
  | otherwise = (Cat (lq s r1) (r2))
lq s (Star r1) = (Cat (lq s r1) (Star (r1)))

nep :: RegExp -> RegExp
nep Empty = Empty
nep (Let c) = Let c
nep (Union r1 r2) = (Union (nep r1) (nep r2))
nep (Cat r1 r2)
  | byp r1 = (Union (Cat (nep r1) (r2)) (nep r2))
  | otherwise = (Cat (nep r1) (r2))
nep (Star r1) = (Cat (nep r1) (Star r1))



  -- Part 1 testing

-- ----------EMPTY---------------
-- testgen empty
-- [False,False,False,False]

-- empty $ toRE "@"
-- True

-- empty $ toRE "ab."
-- False
-- --------UNITARY-----------------
-- testgen unitary
-- [False,False,False,False]

-- unitary $ toRE "@*"
-- True

-- unitary $ toRE "ab."
-- False

-- unitary $ toRE "@**"
-- True

-- unitary $ toRE "@********"
-- True
-- -------------BYP----------------
-- testgen byp
-- [True,False,True,False]

-- byp $ toRE "a*"
-- True

-- byp $ toRE "ab."
-- False

-- byp $ toRE "ab.*"
-- True

-- byp $ toRE "@*"
-- True
-- --------------INF----------------
-- testgen inf
-- [True,True,True,True]

-- inf $ toRE "@*"
-- False

-- inf $ toRE "a*"
-- True

-- inf $ toRE "ab.*"
-- True

-- inf $ toRE "ab."
-- False
-- ---------------REVER------------
-- rever $ toRE "ab."
-- Cat (Let 'b') (Let 'a')

-- rever $ toRE "ab.cd.+"
-- Union (Cat (Let 'b') (Let 'a')) (Cat (Let 'd') (Let 'c'))

-- testgen rever
-- [
-- Star (Union (Cat (Let 'a') (Let 'a')) (Cat (Let 'b') (Let 'b'))),
-- Cat (Union (Let 'a') (Let 'b')) (Cat (Union (Let 'a') (Let 'b')) (Cat (Let 'a') (Star (Union (Let 'a') (Let 'b'))))),
-- Cat (Star (Let 'b')) (Star (Cat (Let 'a') (Cat (Star (Let 'b')) (Cat (Let 'a') (Star (Let 'b')))))),
-- Cat (Star (Union (Let 'a') (Cat (Let 'b') (Let 'a')))) (Cat (Let 'b') (Cat (Let 'b') (Star (Union (Let 'a') (Cat (Let 'a') (Let 'b'))))))
-- ]

-- ---------------lq------------
-- lq 'a' (toRE "ab.")
-- Cat (Star Empty) (Let 'b')

-- lq 'a' (toRE "ab.*")
-- Cat (Cat (Star Empty) (Let 'b')) (Star (Cat (Let 'a') (Let 'b')))

-- (leftq (lol "a") (lang_of $ toRE "ab.")) == (lang_of $ lq 'a' (toRE "ab."))
-- True

-- ---------------nep------------
-- nep $ toRE "@*"
-- Cat Empty (Star Empty)

-- nep $ toRE "a*"
-- Cat (Let 'a') (Star (Let 'a'))

-- nep $ toRE "ab.*"
-- Cat (Cat (Let 'a') (Let 'b')) (Star (Cat (Let 'a') (Let 'b')))

-- nep $ toRE "ab."
-- Cat (Let 'a') (Let 'b')

-- testgen nep
-- [
-- Cat (Union (Cat (Let 'a') (Let 'a')) (Cat (Let 'b') (Let 'b'))) (Star (Union (Cat (Let 'a') (Let 'a')) (Cat (Let 'b') (Let 'b')))),
-- Cat (Cat (Union (Cat (Cat (Union (Let 'a') (Let 'b')) (Star (Union (Let 'a') (Let 'b')))) (Let 'a')) (Let 'a')) (Union (Let 'a') (Let 'b'))) (Union (Let 'a') (Let 'b')),
-- Union (Cat (Cat (Cat (Cat (Union (Cat (Cat (Let 'b') (Star (Let 'b'))) (Let 'a')) (Let 'a')) (Star (Let 'b'))) (Let 'a')) (Star (Cat (Cat (Cat (Star (Let 'b')) (Let 'a')) (Star (Let 'b'))) (Let 'a')))) (Star (Let 'b'))) (Cat (Let 'b') (Star (Let 'b'))),
-- Cat (Cat (Union (Cat (Cat (Union (Let 'a') (Cat (Let 'b') (Let 'a'))) (Star (Union (Let 'a') (Cat (Let 'b') (Let 'a'))))) (Let 'b')) (Let 'b')) (Let 'b')) (Star (Union (Let 'a') (Cat (Let 'a') (Let 'b'))))
-- ]

---------------- Part 2 ----------------

-- Implement the two matching algorithms given in Section 3.4 of the notes.
-- Call them match1 and match2. Start by implementing splits:

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]
splits :: [a] -> [([a], [a])]
splits xs = [(take x xs, drop x xs) | x <- [0..(length xs)]]

match1 :: RegExp -> String -> Bool
match1 Empty w = False
match1 (Let c) "" = False
match1 (Let c) (w:ws) = c == w && ws == []
match1 (Union r1 r2) w = (match1 r1 w) || (match1 r2 w)
match1 (Cat r1 r2) w = or [ (match1 r1 w1) && (match1 r2 w2) | (w1, w2) <- (splits w) ]
match1 (Star r1) w = w == "" || or [(match1 r1 w1) && (match1 (Star r1) w2) | (w1, w2) <- (splits w), w1 /= ""]


match2 :: RegExp -> String -> Bool
match2 r w = undefined



-- Some regular expressions for testing. Also, test them on other solutions
-- to the exercises of Section 3.2 (as many as you can get). 

sigma = ['a', 'b']                -- Alphabet used in all examples below

ab   = toRE "aa.bb.+*"            -- every letter is duplicated
ttla = toRE "ab+*a.ab+.ab+."      -- third to last letter is a
ena  = toRE "b*a.b*.a.*b*."       -- even number of a's
bb1  = toRE "aba.+*b.b.aab.+*."   -- contains bb exactly once

testList = [ab,ttla,ena,bb1]
testgen f = [ f re | re <- testList]  -- for testing part 1

-- For your tests, you may also find the following helpful. For example,
-- you can generate all strings of length 10 (or 20) or less and then test
-- to see whether match1 r w == memb (lol w) (lang_of r) for each such w.

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]


-- TESTING PART 2

-- and [match1 r w == memb (lol w) (lang_of r)  | w <- strings 5, r <- testList]
-- True

-- and [match1 r w == memb (lol w) (lang_of r)  | w <- strings 7, r <- testList]
-- True
