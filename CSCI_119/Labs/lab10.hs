-- Bryce Remick

import Data.List
import Data.Array
import Control.Monad (replicateM)

-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)

-- Nondeterministic FSMs, indexed by their type of state
-- All states are normalized and the output of d is normalized
-- M = (states, starts, finals, transitions)  
type Trans a = a -> Char -> [a]
type NFSM a = ([a], [a], [a], Trans a)

sigma :: [Char]
sigma = "ab"

-- Cartesian product (preserves normalization)
(><) :: [a] -> [b] -> [(a,b)]
xs >< ys = [(x,y) | x <- xs, y <- ys]

accept2_aux :: Eq a => FSM a -> a -> [Char] -> Bool
accept2_aux m@(_, _, fs, _) q [] = elem q fs
accept2_aux m@(_, _, _, d) q (a:w) = accept2_aux m (d q a) w

accept2 :: Eq a => FSM a -> [Char] -> Bool
accept2 m@(_, s, _, _) w = accept2_aux m s w

uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr

norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys



------------------LAB 10 STARTS HERE------------------------------------------

-- Boolean binary operation on FSMs. Examples:
-- binopFSM (||) m1 m2 computes union machine
-- binopFSM (&&) m1 m2 computes intersection machine
binopFSM :: (Eq a, Eq b) => (Bool -> Bool -> Bool) -> FSM a -> FSM b -> FSM (a,b)
binopFSM op (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
    qs = qs1 >< qs2
    s  = (s1, s2)
    fs = [(q1, q2) | q1 <- qs1, q2 <- qs2, op (q1 `elem` fs1) (q2 `elem` fs2)]
    d (qs1, qs2) a = (d1 qs1 a, d2 qs2 a)
  

-- Reverse FSM to a NFSM. Output machine accepts reverse of language of input machine.
reverseFSM :: Eq a => FSM a -> NFSM a
reverseFSM (qs1, s1, fs1, d1) = (qs, s, fs, d) where 
    qs = qs1
    s = fs1
    fs = [s1]
    d q a = [q' | q' <- qs1, q == (d1 q' a)]


-- Reachable states of a NFSM (similar to reachable but on NFSMs)
-- start states & delta different
nreachable :: Ord a => NFSM a -> [a]
nreachable (qs, s, fs, d) = (uclosure s f) where 
    f q = norm $ concat (map (d q) sigma)
  

-- Minimize a FSM. Put all of the above together to compute the minimum machine for m
minimize :: Ord a => FSM a -> FSM a
minimize (qs, s, fs, d) = (qs', s', fs', d') where
    xor = (binopFSM (/=) (qs, s, fs, d) (qs, s, fs, d))
    reverse = reverseFSM xor
    nreach = nreachable reverse
    compliment = [(q1, q2) | q1 <- qs, q2 <- qs, (notElem (q1, q2) nreach)]
    rep r = minimum [q2| (q1, q2) <- compliment, q1 == r]
    qs' = norm [rep q| q <- qs]
    s' = rep s
    fs' = intersect (norm [rep q| q <- qs]) fs
    d' q a = rep (d q a)
 

-----------------------TESTING----------------------------------------

-- Test. For example, make sure that the minimal machine agrees with the original machine
-- on lots of input strings. Try for multiple machines.

-- test fsms

strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

letA :: FSM Int
letA = ([0,1,2], 0, [1], d) where
    d 0 'a' = 1
    d _ _ = 2

letB :: FSM Int
letB = ([0,1,2], 0, [1], d) where
    d 0 'b' = 1
    d _ _ = 2

onlyA :: FSM Int
onlyA = ([0,1,2], 0, [1], f) where
    f 2 _ = 2
    f q 'a' = q + 1
    f q _ = q  
    
onlyB :: FSM Int
onlyB = ([0,1,2], 0, [1], f) where
    f 2 _ = 2
    f q 'b' = q + 1
    f q _ = q  

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



-- TESTING
 
testFSMs = [letA, letB, odd_bs, onlyA, onlyB]   -- int
testFSMs2 = [ends_in_ab, ends_in_bc]            -- string

test1 = and [(accept2 fsm s) == (accept2 (minimize fsm) s) | fsm <- testFSMs, s <- strings 4]
test2 = and [(accept2 fsm s) == (accept2 (minimize fsm) s) | fsm <- testFSMs2, s <- strings 4]

{-
*Main> test1
True

*Main> test2
True
-}