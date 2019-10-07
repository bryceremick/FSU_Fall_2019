-- CSci 119, Lab 5
-- Reference: Lecture notes, Sections 4.1, 4.2
import Data.List (sort, nub)

-- Again, for this (and most) labs, the alphabet will be {a,b} to keep testing
-- easier, but your code should work just as well for any finite list.
sigma = ['a', 'b']

-- Finite State Machine M = (Q, s, F, d) with integer states
-- ([0,1,2], 0 , [1], ts) -> see picture of board
-- ts :: Int -> Char -> Int (State -> Char -> State)
-- ts 0 'b' -> 0
-- ts 0 'a' -> 1
-- ...
-- ts 2 'b' -> 2x
type FSM = ([Int], Int, [Int], Int -> Char -> Int)

onlyA :: FSM
onlyA = ([0,1,2], 0, [1], f) where
    f 2 _ = 2
    f q 'a' = q + 1
    f q _ = q


noDup :: [Int] -> Bool
noDup [] = True
noDup (x:xs)
    | x `elem` xs = False
    | otherwise = noDup xs

-- noDup' :: [Int] -> Bool
-- noDup' xs = xs == nub xs

subset :: [Int] -> [Int] -> Bool
subset a b = all (`elem` b) a

checkTransFunc :: (Int -> Char -> Int) -> [Int] -> Bool
checkTransFunc ts qs = and [ (ts q c) `elem` qs | q <- qs, c <- sigma]

-- Check whether a finite state machine (qs, s, fs, ts) is correct/complete:
-- (1) States qs are unique (no duplicates)
-- (2) Start state is a state (s is in qs)
-- (3) Final states are states (fs is a subset of qs)
-- (4) Transition function gives a state in qs for every state in qs and
--     letter from sigma (use list comprehension and [ ts q let | q <- qs, let <- sigma])
-- similar to "Part" func
checkFSM :: FSM -> Bool
checkFSM (qs, s, fs, ts) = (noDup qs) && (s `elem` qs) && (fs `subset` qs) && (checkTransFunc ts qs)

-- Gives the delta* function (recursive in w)
delta_star :: FSM -> Int -> [Char] -> Int -- is Int param the start state?
delta_star m q [] = q
delta_star m@(qs,s,fs,ts) q w@(x:xs) = delta_star (m) (ts q x) (xs)

-- Machine acceptance, Definition 1 (via delta*)
accept1 :: FSM -> [Char] -> Bool
accept1 m@(qs,s,fs,ts) w = (delta_star m s w) `elem` fs


-- Machine acceptance, Definition 2 (via L_q(M))

-- accept2_aux m q w = whether m, starting in q, accepts w (recursive in w)
accept2_aux :: FSM -> Int -> [Char] -> Bool
accept2_aux m@(qs,s,fs,ts) q [] = q `elem` fs
accept2_aux m@(qs,s,fs,ts) q w@(x:xs) = accept2_aux (m) (ts q x) (xs)

-- Acceptance, defined (non-recursively) in terms of accept2_aux
accept2 :: FSM -> [Char] -> Bool
accept2 m@(qs,s,fs,ts) w@(x:xs) = accept2_aux m s w


---- FSM construction

-- Define a machine that accepts exactly the strings with an odd number of b's
-- and test it adequately
oddbs :: FSM
oddbs = undefined

-- Define a machine that accepts exactly the strings that do not contain "aab"
-- as a substring and test it adequately
avoid_aab :: FSM
avoid_aab = undefined

-- Define a machine that accepts all strings that end in "ab" and test
end_ab :: FSM
end_ab = undefined

-- Define a function that takes a string and returns a machine that excepts
-- exactly that string and nothing else
exactly :: String -> FSM
exactly s = undefined

-- test1 = and [accept1 oddbs x == accept2 oddbs x | x <- strings 5]

{-


-}