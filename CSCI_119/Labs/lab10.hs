-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)

-- Nondeterministic FSMs, indexed by their type of state
-- All states are normalized and the output of d is normalized
-- M = (states, starts, finals, transitions)  
type Trans a = a -> Char -> [a]
type NFSM a = ([a], [a], [a], Trans a)


-- Boolean binary operation on FSMs. Examples:
-- binopFSM (||) m1 m2 computes union machine
-- binopFSM (&&) m1 m2 computes intersection machine
binopFSM :: (Eq a, Eq b) => (Bool -> Bool -> Bool) -> FSM a -> FSM b -> FSM (a,b)
binopFSM op m1 m2 = undefined

-- Reverse FSM to a NFSM. Output machine accepts reverse of language of input machine.
reverseFSM :: Eq a => FSM a -> NFSM a
reverseFSM m = undefined

-- Reachable states of a NFSM (similar to reachable but on NFSMs)
-- start states & delta different
nreachable :: Ord a => NFSM a -> [a]
nreachable m = undefined

-- Minimize a FSM. Put all of the above together to compute the minimum machine for m
minimize :: Ord a => FSM a -> FSM a
minimize m = undefined

-- Test. For example, make sure that the minimal machine agrees with the original machine
-- on lots of input strings. Try for multiple machines.