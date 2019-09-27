-- CSci 119, Lab 3
-- Bryce Remick
-- 109557467

-- See http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-List.html
import Data.List (sort, stripPrefix, nub)
import Data.Maybe


---------------- General list functions

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product, preserves normalization
cart :: [a] -> [b] -> [(a,b)]
cart xs ys = [(a,b) | a <- xs, b <- ys]


-- Powerset, preserves normalization. Examples:
-- power [] = [[]]
-- power [1] = [[],[1]]
-- power [1,2] = [[],[1],[1,2],[2]]
-- power [1,2,3] = [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
-- build upon previous lists, use variable in body (let ps = ...... in)
powerh :: [a] -> [[a]]
powerh [] = [[]]
powerh (x:xs) = [] : map (x:) (powerh xs) ++ powerh xs
power xs = nub $ powerh xs -- ¯\_(ツ)_/¯


---------------- Length-ordered lists

-- Length-Ordered Lists over "character type" a (aka "strings")
-- Invariant: In LOL n xs, n == length xs
data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

-- testing stuff
l1 = LOL 3 "wow"
l2 = LOL 4 "zers"
lenLOL (LOL n xs) = n

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Concatenation of LOLs, preserves invariant (one line)
dot :: LOL a -> LOL a -> LOL a
dot (LOL n1 xs) (LOL n2 ys) = LOL (n1 + n2) (xs++ys)

-- Reverse of LOLs, preserves invariant (one line)
rev :: LOL a -> LOL a
rev (LOL n xs) = LOL n (reverse xs)



---------------- Languages

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is ordered with no duplicates
type Lang a = [LOL a]

-- Constructor for languages
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

lang1 = lang ["aa", "ab", "aaa", "abc"]
lang2 = lang ["b", "aa", "ab", "aa"]

-- Merge of langages (aka "union") (hard)
merge :: Ord a => Lang a -> Lang a -> Lang a
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | x == y = merge xs (y:ys) -- skip if equal (avoids duplicates)
  | otherwise = y:merge (x:xs) ys
merge' xs ys = norm $ merge xs ys -- for testing

-- Helper function for cat. concats all elements in first param
-- with FIRST element in 2nd param
catHelper :: Ord a => Lang a -> Lang a -> Lang a
catHelper [] ys = []
catHelper (x:xs) (y:ys) = (x `dot` y):catHelper xs (y:ys)

-- Concatenation of languages (hard)
-- Applies catHelper function to all elements in second param
cat :: Ord a => Lang a -> Lang a -> Lang a
cat xs [] = []
cat (x:xs) (y:ys) = catHelper (x:xs) (y:ys) ++ cat (x:xs) ys
cat' xs ys = norm $ cat xs ys -- for testing

-- Kleene star of languages
kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr 
kstar xs = eps : cat xs (kstar xs)
-- kstar' xs = norm $ kstar xs

-- helper func for leftq, simply does stripPrefix on two LOLs
stripPrefixLOL (LOL n1 xs) (LOL n2 ys) = stripPrefix xs ys

-- Left quotient of a language by an LOL (cf. Definition 2.16)
-- Hint: Use the stripPrefix function
leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq x [] = []
leftq x (y:ys)
    | isJust z = (lol $ fromJust z):leftq x ys
    | otherwise = leftq x ys
    where 
      z = stripPrefixLOL x y


---- Regular expressions and the languages they denote 
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



-- "ab.bc.+*"
-- Star (Union (Cat (Let 'a') (Let 'b')) (Cat (Let 'b') (Let 'c')))

-- Cat (Cat (Let 'a') (Let 'b')) (Cat (Let 'b') (Let 'c'))
-- Cat (Let 'a') (Let 'b')

-- The language associated to a regular expression, i.e., [[r]]
lang_of :: RegExp -> Lang Char
lang_of Empty = lang []
lang_of (Let c) = [lol [c]]
lang_of (Cat r1 r2) = (lang_of r1) `cat` (lang_of r2)
lang_of (Union r1 r2) = (lang_of r1) `merge` (lang_of r2)
lang_of (Star r1) = kstar (lang_of r1)



-- The one-string and finite languages of Theorem 3.2. It should be the case
-- that, for any string w, lang_of (onestr w) == [w], and for any (finite) list
-- of (distinct) strings l, lang_of (finite l) == l.
onestr :: String -> RegExp
onestr (x:[]) = Let x
onestr (x:xs) = (Cat (Let x) (onestr xs)) 

finite :: [String] -> RegExp
finite (x:[]) = onestr x
finite (x:xs) = (Union (onestr x) (finite xs))


-- Test all of the above operations extensively!            

-- -------------------------------------------------------

-- cart [1..3] [4..7]
-- [(1,4),(1,5),(1,6),(1,7),(2,4),(2,5),(2,6),(2,7),(3,4),(3,5),(3,6),(3,7)]

-- power [1,2,3]
-- [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]

-- lol "abc" `dot` lol "def"
-- "abcdef"

-- lenLOL $ lol "abc" `dot` lol "def"
-- 6

-- rev $ lol "abc"
-- "cba"

-- lenLOL $ rev $ lol "abc"
-- 3

-- (LOL 3 "aaa") < (LOL 2 "bb")
-- False

-- (LOL 1 "a") < (LOL 1 "c")
-- True

-- (LOL 6 "abcdef") < (LOL 4 "cdef")
-- False

-- (lol "abc") < (lol "d")
-- False

-- -------------------------------------------------------

-- (Declare languages for testing and convenience)
-- lang1 = lang ["aa", "ab", "aaa", "abc"]
-- lang2 = lang ["b", "aa", "ab", "aa"]

-- merge lang1 lang2
-- ["b","aa","ab","aaa","abc"]

-- merge (lang ["aa", "ab"]) (lang ["b", "aa"])
-- ["b","aa","ab"]

-- cat lang1 lang2
-- ["aab","abb","aaab","abcb","aaaa","abaa","aaaaa","abcaa","aaab","abab","aaaab","abcab"]

-- cat (lang ["aa", "ab"]) (lang ["b", "aa"])
-- ["aab","abb","aaaa","abaa"]

-- take 5 $ kstar (lang ["b", "aa"])
-- ["","b","aa","bb","aab"]

-- leftq (LOL 1 "a") (lang ["b", "aa"])
-- ["a"]

-- leftq (LOL 1 "a") (lang1)
-- ["a","b","aa","bc"]

-- -------------------------------------------------------

-- lang_of $ toRE "ab.bc.+"
-- ["ab","bc"]

-- lang_of $ toRE "abbc..+"
-- ["a","bbc"]

-- lang_of $ toRE "ab.bc.."
-- ["abbc"]

-- take 30 (lang_of $ toRE "ab.bc.+*")
-- ["","ab","bc","abab","bcab","abbc","bcbc","ababab","bcabab",
-- "abbcab","bcbcab","ababbc","bcabbc","abbcbc","bcbcbc","abababab","bcababab","abbcabab","bcbcabab",
-- "ababbcab","bcabbcab","abbcbcab","bcbcbcab","abababbc","bcababbc","abbcabbc","bcbcabbc","ababbcbc",
-- "bcabbcbc","abbcbcbc"]

-- onestr "abc"
-- Cat (Let 'a') (Cat (Let 'b') (Let 'c'))

-- onestr "abcdef"
-- Cat (Let 'a') (Cat (Let 'b') (Cat (Let 'c') (Cat (Let 'd') (Cat (Let 'e') (Let 'f')))))

-- lang_of (onestr "abc")
-- ["abc"]

-- finite ["aba", "c"]
-- Union (Cat (Let 'a') (Cat (Let 'b') (Let 'a'))) (Let 'c')

-- finite ["aba", "c", "ef", "z"]
-- Union (Cat (Let 'a') (Cat (Let 'b') (Let 'a'))) (Union (Let 'c') (Union (Cat (Let 'e') (Let 'f')) (Let 'z')))

-- lang_of (finite ["aba", "c"])
-- ["c","aba"]