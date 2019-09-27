---- CSci 119, Fall 2019, Lab 2 ----

-- As in Lab 1, we will be working with the finite universe U = [1..8]
u = [1..8]


----- PART 1:  Relations on u -----

-- A relation, R, on U is a list of the ordered pairs of elements of U:
type Reln = [(Int,Int)]
              
-- For example, here are the < and <= relations
less_reln :: Reln
less_reln = [(i,j) | j <- [1..8], i <- [1..j-1]]

leq_reln :: Reln
leq_reln  = [(i,j) | j <- [1..8], i <- [1..j]]
            
-- and here is the relation of equivalence mod 3:
eqmod3_reln :: Reln
eqmod3_reln = [(i,j) | i <- [1..8], j <- [1..8], (j - i) `mod` 3 == 0]

-- Function to test if 'a' is a subset(or member) of 'b' 
-- returns true if a is subset of b, and false if not
belongsTo :: Reln -> Reln -> Bool
belongsTo a b = all (`elem` b) a

-- member x [] = False
-- member x (y:ys) = if x == y then True else member x ys

-- Write a function refl that tests whether a relation is reflexive:
-- R is reflexive if: forall a, (a,a) in R
-- Example: [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)] is the
-- smallest reflexive relation over u. Anything that does not contain
-- these 8 elements is not reflexive
refl :: Reln -> Bool
refl rs = and [[(x, x)] `belongsTo` rs  | (x,y) <- rs]
refl' rs = and [[(x,x)] `belongsTo` rs | x <- u]

-- Write a function symm that tests whether a relation is symmetric:
-- R is symmetric if: forall a b, (a,b) in R -> (b,a) in R
-- Example: [(1,1), (1,2), (2,1)] is symmetric but [(1,1), (1,2)] is not.
symm :: Reln -> Bool
symm rs = and [ [(y, x)] `belongsTo` rs | (x, y) <- rs]

-- Write a function trans that tests whether a relation is transitive:
-- R is transistive if: forall a b c, (a,b) in R /\ (b,c) in R -> (a,c) in R
-- Example: [(1,2),(2,3),(1,3),(4,4)] is transitive but [(2,3),(3,2)] is not
trans :: Reln -> Bool
trans rs = and [ [(x, z)] `belongsTo` rs | (x,y1) <- rs, (y2,z) <- rs, y1 == y2]
-- trans rs = and [ [(x, z)] `belongsTo` rs | (x,y) <- rs, (y,z) <- rs]
-- test rs = [(x,z) | (x,y1) <- rs, (y2,z) <- rs, y1 == y2]
-- test rs = [[(x,y),(y,z)] | (x,y) <- rs, (y,z) <- rs]


-- Use the functions above to check the less, leq, and eqmod3 relations for
-- relexivity, symmetry, and transitivity

-- refl less : false
-- refl leq : true
-- refl eqmod3 : true
----------------------
-- symm less : false
-- symm leq : false
-- symm eqmod3 : true
----------------------
-- trans less : true
-- trans leq : true
-- trans eqmod3 : true

-- For each of the 8 possible combinations of yes/no on reflexivity,
-- symmetry, and transitivity, find a MINIMAL relation on u that has exactly
-- that combination of properties. Add a test to check whether you got the
-- properties right. (I'll do the first one as an example.)

-- refl, symm, trans
rst :: Reln
rst = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8)]
rst_test = refl rst && symm rst && trans rst

-- refl, symm, not trans
rst' :: Reln
rst' = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,2),(2,3),(2,1),(3,2)]
rst'_test = refl rst' && symm rst' && not (trans rst')

-- refl, not symm, trans
rs't :: Reln
rs't = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,2)]
rs't_test = refl rs't && not (symm rs't) && trans rs't

-- refl, not symm, not trans
rs't' :: Reln
rs't' = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,2),(2,3)]
rs't'_test = refl rs't' && not (symm rs't') && not (trans rs't')

-- not refl, symm, trans
r'st :: Reln
r'st = []
r'st_test = not (refl' r'st) && symm r'st && trans r'st

-- not refl, symm, not trans
r'st' :: Reln
r'st' = [(1,2),(2,1)]
r'st'_test = not (refl' r'st') && symm r'st' && not (trans r'st')

-- not refl, not symm, trans
r's't :: Reln
r's't = [(1,2),(2,3),(1,3)]
r's't_test = not (refl' r's't) && not (symm r's't) && trans r's't

-- not refl, not symm, not trans
r's't' :: Reln
r's't' = [(1,2),(2,3)]
r's't'_test = not (refl' r's't') && not (symm r's't') && not (trans r's't')


---- PART 2: Partitions of u -----

-- A partitition, P, of u is a list of blocks, which are lists of elements
-- of u, that satisfies certain these conditions:
-- nontrivial: forall X in P, exists x in U, x in X, or
--             {} not in P
-- total: forall x in U, exists X in P, x in X
-- disjoint: forall X,Y in U (exists a, a in X /\ a in Y) -> X = Y, or
--           forall X,Y in U, X /= Y -> X intersect Y = {}

-- For example, here is the partitition of u corresponding to equivalence mod 3:
eqmod3_part :: [[Int]]
eqmod3_part = [[1,4,7], [2,5,8], [3,6]]

-- flatten :: [[a]] -> [a]
-- flatten [] = []
-- flatten [[a]] = [a]
-- flatten (x: xs) = x ++ flatten xs


------------Helper Functions-------------------
trivial :: [[Int]] -> Bool
trivial bs = [] `elem` bs

total :: [[Int]] -> Bool
total bs = and [or [(a `elem` x)| x <- bs] | a <- u]

disjoint :: [[Int]] -> Bool
disjoint bs = and [(x == y) || [z `elem` x && z `elem` y | z <- x,  _ <- y] == [] | x <- bs, y <- bs]
-----------------------------------------------

-- Write a function part that tests whether a list of lists is a partition of u
part :: [[Int]] -> Bool
part bs = not (trivial bs) && total bs && disjoint bs


-- test bs = and [x `elem` flatten bs | x <- u]


-- Write a function eq2part that takes an equivalence relation on u as input
-- and returns the associated partition of u. You can assume that the input is
-- really an equivalence relation on u.
eq2part :: Reln -> [[Int]]
eq2part rs = undefined


-- Write a function part2eq that takes a partition of u as input and returns
-- the associated equivalence relation on u. You can assume that the argument
-- is really a partition of u.
part2eq :: [[Int]] -> Reln
part2eq bs = undefined

