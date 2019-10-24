-- Lab 8 Solution: Additional constructions, nondeterministic machines

import Data.List
import Data.Array
import Control.Monad (replicateM)  -- for strings function below

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)

letA :: FSM Int
letA = ([0,1,2], 0, [1], d) where
    d 0 'a' = 1
    d _ _ = 2

letB :: FSM Int
letB = ([0,1,2], 0, [1], d) where
    d 0 'b' = 1
    d _ _ = 2

ends_in :: String -> FSM String
ends_in xs = (qs, "", [reverse xs], d) where
  n = length xs
  qs = strings n
  d w a = take n (a : w)

-- ends_in_ab is a machine that accepts strings ending in "ab"
ends_in_ab :: FSM String
ends_in_ab = ends_in "ab"

ends_in_bc :: FSM String
ends_in_bc = ends_in "bc"

odd_bs :: FSM Int
odd_bs = ([0,1], 0, [1], d) where
  d q a = if a == 'b' then (q+1) `mod` 2 else q

accept2_aux :: Eq a => FSM a -> a -> [Char] -> Bool
accept2_aux m@(_, _, fs, _) q [] = elem q fs
accept2_aux m@(_, _, _, d) q (a:w) = accept2_aux m (d q a) w

accept2 :: Eq a => FSM a -> [Char] -> Bool
accept2 m@(_, s, _, _) w = accept2_aux m s w


---------------- Given functions ----------------

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product (preserves normalization)
(><) :: [a] -> [b] -> [(a,b)]
xs >< ys = [(x,y) | x <- xs, y <- ys]

-- Powerset  (preserves normalization)
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs
               in [] : map (x:) ys ++ tail ys

-- Check whether two lists overlap
overlap :: Eq a => [a] -> [a] -> Bool
overlap [] ys = False
overlap (x:xs) ys = elem x ys || overlap xs ys

-- delta* construction
star :: (a -> Char -> a) -> (a -> [Char] -> a)
star = foldl'

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


---- Regular expressions, along with output and input
data RegExp = Empty
             | Let Char
             | Union RegExp RegExp
             | Cat RegExp RegExp
             | Star RegExp
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

-- Quick and dirty postfix regex parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = toRE' w [] where
  toRE' [] [r] = r
  toRE' ('+':xs) (r2:r1:rs) = toRE' xs (Union r1 r2:rs)
  toRE' ('.':xs) (r2:r1:rs) = toRE' xs (Cat r1 r2:rs)
  toRE' ('*':xs) (r:rs) = toRE' xs (Star r:rs)
  toRE' ('@':xs) rs = toRE' xs (Empty:rs)
  toRE' (x:xs) rs = toRE' xs (Let x:rs)


---------------- Part 1: Additional constructions ----------------
-- Define the operations given in Section 7 of the notes

-- Intersection -- opposite of union (and instead of or)
inters :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a,b)
inters (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< qs2
  s = (s1,s2)
  fs = [(q1,q2) | q1 <- qs1, q2 <- qs2, (q1 `elem` fs1 && q2 `elem` fs2)]
  d (q1,q2) a = ((d1 q1 a),(d2 q2 a))
  

-- Complement - eg. letA -> accepts everything that is NOT a
compl :: Eq a => FSM a -> FSM a
compl (qs1, s1, fs1, d1) = (qs, s, fs, d) where
  qs = qs1
  s = s1
  fs = [q | q <- qs1, (q `elem` qs1 && not (q `elem` fs1))]
  d = d1


onestr :: String -> RegExp
onestr [] = Star Empty
onestr [x] = Let x
onestr (x:xs) = Cat (Let x) (onestr xs)

f :: (Char -> String)
f a = [a] ++ [a]
-- Direct homomorphic image: k is a substitution
-- use onestr on letter case
-- create any function that converts char -> string
homo_dir :: (Char -> String) -> RegExp -> RegExp
homo_dir f (Empty) = Empty
homo_dir f (Let c) = onestr (f c)
homo_dir f (Union r1 r2) = Union (homo_dir f r1) (homo_dir f r2)
homo_dir f (Cat r1 r2) = Cat (homo_dir f r1) (homo_dir f r2)
homo_dir f (Star r) = Star (homo_dir f r)

-- Inverse homomorphic image
-- FSM delta different (pg 48 very end)
homo_inv :: Eq a => (Char -> String) -> FSM a -> FSM a
homo_inv f (qs1, s1, fs1, d1) = (qs, s, fs, d) where
  qs = qs1
  s = s1
  fs = fs1
  d q a = star q (f a)

-- Right quotient
-- match suffixes. if suffix matches, take the prefix(put in returned language)
-- (pg 49 before sect 8)
quot_right :: Eq a => [String] -> FSM a -> FSM a
quot_right = undefined


---------------- Part 2: Nondeterministic machines ----------------

-- test these using part 3

-- Nondeterministic FSMs, indexed by their type of state
-- All states are normalized and the output of d is normalized
-- M = (states, starts, finals, transitions)  
type Trans a = a -> Char -> [a]
type NFSM a = ([a], [a], [a], Trans a)

-- nap_hat d qs a == normalized list of all transitions possible from qs on a
-- use concat and norm
nap_hat :: Ord a => Trans a -> [a] -> Char -> [a]
nap_hat = undefined

-- nap_hat_star d qs w == normalized list of transitions possible from qs on w
-- page 50 "We then define.."
nap_hat_star :: Ord a => Trans a -> [a] -> [Char] -> [a]
nap_hat_star = undefined

-- nacc m w == m accepd the string w
nacc :: Ord a => NFSM a -> [Char] -> Bool
nacc = undefined


-- Nondeterministic FSMs with epsilon moves, indexed by their type of state
-- M = (states, starts, finals, transitions, epsilon-moves)
type Eps a = [(a, a)]
type EFSM a = ([a], [a], [a], Trans a, Eps a)

-- Normalized epsilon closure of a set of states (Hint: use uclosure)
-- loop thru Eps a apply uclosure to x, and return y where x = q
eclose :: Ord a => Eps a -> [a] -> [a]
eclose = undefined
  
-- eap_has d es qs a == eclosure of transitions possible from qs on a
eap_hat :: Ord a => (Trans a, Eps a) -> [a] -> Char -> [a]
eap_hat = undefined

eacc :: Ord a => EFSM a -> [Char] -> Bool
eacc = undefined



---------------- Part 3: Conversion between machines ----------------

-- Easy conversion from FSM to NFSM (given)
fsm_to_nfsm :: Eq a => FSM a -> NFSM a
fsm_to_nfsm = undefined


-- Conversion from NFSM to FSM by the "subset construction"
nfsm_to_fsm :: Ord a => NFSM a -> FSM [a]
nfsm_to_fsm = undefined


-- Similar conversion from EFSM to FSM using epsilon closure
efsm_to_fsm :: Ord a => EFSM a -> FSM [a]
efsm_to_fsm = undefined


-- PART 1 TESTS

{-
*Main> let f = inters odd_bs ends_in_ab
*Main> accept2 f "ab"
True
*Main> accept2 f "abab"
False
*Main> accept2 f "bbab"
True
*Main> accept2 f "bbbab"
False
*Main> accept2 f "babb"
False
-}

{-
*Main> let f = compl letA
*Main> accept2 f "a"
False
*Main> accept2 f "b"
True
*Main> accept2 f "c"
True
*Main> accept2 f "d"
True
*Main> accept2 f "abc"
True
-}

{-
*Main> Compact $ homo_dir f (toRE "ab.cd.+")
aabb+ccdd
*Main> Compact $ homo_dir f (toRE "ab.cd.+*")
(aabb+ccdd)*
*Main> Compact $ homo_dir f (toRE "ab.cd.ef...")
aabbccddeeff
-}


{- Tests:

1. m and fsm_to_nfsm m accept the same strings
2. m and nfsm_to_fsm m accept the same strings
3. m and efsm_to_fsm m accept the same strings

-}
