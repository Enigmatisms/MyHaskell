argminWithFlag :: (Ord a, Num a) => [a] -> [Bool] -> a -> Int -> Int -> Int
argminWithFlag [] [] min_val min_pos idx = min_pos
argminWithFlag (x:xs) (y:ys) min_val min_pos idx
    | ((x < min_val) && y)  = (argminWithFlag xs ys x idx (idx + 1))
    | otherwise             = (argminWithFlag xs ys min_val min_pos (idx + 1))

argmin :: (Ord a, Num a) => [a] -> [Bool] -> Int
argmin vals flags = (argminWithFlag vals flags 1000 (-1) 0)

arr = [1, 3, 1, 5, 0, 3, 7, 0, 2]
flags = [False, True, True, True, True, True, True, False, True]
main = putStrLn (show (argmin arr flags))