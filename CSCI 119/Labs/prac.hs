a1 = [1..10]
a2 = [5..15]

-- diff xs [] = xs
diff (x:xs) (y:ys)
    | (x:xs) == (y:ys) = (x:xs)
    | x `elem` (y:ys) = diff xs (y:ys)
    | y `elem` (x:xs) = diff (x:xs) ys
    | otherwise = diff (x:xs) (y:ys)

diff' xs ys = [x | x <- xs, not (x `elem` ys)]

test = [1,2,3] == [1,2,3]