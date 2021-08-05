-- Binary search implemented in haskell
-- 所有循环都需要使用递归实现

intMean :: Int -> Int -> Int
intMean x y = (x + y) `div` 2   -- 开始用了floor， 以及 /，最后`div`才得结果
{- 很显然：
 :t div 发现是 div :: Integral a => a -> a -> a
 :t (/) (/) :: Fractional a => a -> a -> a
 所以开始的实现，无法让“/”起作用，算出的结果
-}

binarySearch :: [Int] -> Int -> Int -> Int -> Int
binarySearch input val s e
    | e == s                    = s
    | ((input !! m) > val)      = (binarySearch input val s m)
    | ((input !! m) < val)      = (binarySearch input val (m + 1) e)
    | otherwise                 = m
    where m = intMean s e

main = putStrLn (show (binarySearch [2 * x | x <- [1..20]] 100 0 19))