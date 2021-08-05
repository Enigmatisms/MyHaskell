-- Dijkstra Algorithm --

makeArray :: (Ord a) => Int -> a -> [a] -> [a]
makeArray pos val arr = 
    front ++ (val:back)
    where len = (length arr) - 1
          front = take pos arr
          back = drop (pos + 1) arr

changeVal :: (Ord a, Num a) => [[a]] -> [a] -> [Bool] -> Int -> Int -> [a]
changeVal adjs vals flags min_pos pos
    | (new_val < (vals !! pos))     = (makeArray pos new_val vals)
    | otherwise                     = vals
    where min_val = (vals !! min_pos)
          adj = (adjs !! min_pos)
          new_val = (min_val + (adj !! pos))

updateRow :: (Ord a, Num a) => [[a]] -> [a] -> [Bool] -> Int -> Int -> [a]
updateRow adjs vals flags min_pos cnt
    | ((cnt < len) && (flags !! cnt))       = (updateRow adjs (changeVal adjs vals flags min_pos cnt) flags min_pos (cnt + 1))
    | ((cnt < len) && (not (flags !! cnt))) = (updateRow adjs vals flags min_pos (cnt + 1))     -- flag = False, no change --
    | otherwise                             = vals            
    where len = (length vals)

argminWithFlag :: (Ord a, Num a) => [a] -> [Bool] -> a -> Int -> Int -> Int
argminWithFlag [] [] min_val min_pos idx = min_pos
argminWithFlag (x:xs) (y:ys) min_val min_pos idx
    | ((x < min_val) && y)  = (argminWithFlag xs ys x idx (idx + 1))
    | otherwise             = (argminWithFlag xs ys min_val min_pos (idx + 1))

argmin :: (Ord a, Num a) => [a] -> [Bool] -> Int
argmin vals flags = (argminWithFlag vals flags 1000 (-1) 0)

recurBody :: (Ord a, Num a) => [[a]] -> [a] -> [Bool] -> Int -> [a]
recurBody adjs vals flags cnt = 
    if ((cnt < len) && (min_pos >= 0)) then do
        (recurBody adjs new_vals new_flags (cnt + 1))
    else do
        vals
    where min_pos = (argmin vals flags)     -- 为-1说明全为False
          len = (length vals)
          new_flags = (makeArray min_pos False flags)
          new_vals = (updateRow adjs vals new_flags min_pos 0)

dijkstra :: (Ord a, Num a) => [[a]] -> [a]
dijkstra adjs = (recurBody adjs vals flags 0)
    where len = (length adjs)
          vals = 0:(take (len - 1) (repeat 1000))
          flags = (take len (repeat True))

-- main --
adjs = [[0, 1, 3, 1000, 1000, 1000], [1, 0, 1000, 2, 1, 1000], [3, 1000, 0, 2, 2, 1000], [1000, 2, 2, 0, 1000, 3], [1000, 1, 2, 1000, 0, 1], [1000, 1000, 1000, 3, 1, 0]]
main = (putStrLn (show (dijkstra adjs)))