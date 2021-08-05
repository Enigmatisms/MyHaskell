-- 质数生成器
-- 质数x：小于等于 sqrt x的数中，没有可以整除x的

exactDiv :: (Integral a) => a -> a -> a -> Bool
exactDiv now ub val
    | (now > ub)            = True
    | (flag == False)       = (exactDiv (now + 1) ub val)
    | otherwise             = False
    where flag = ((val `mod` now) == 0)

isPrime :: (Integral a) => a -> Bool
isPrime x = (exactDiv 2 ub x)
    where ub = floor(sqrt (fromIntegral x))

primeGen :: (Integral a) => a -> [a]
primeGen x = [num | num <- [2..x], ((isPrime num) == True)]

main = putStrLn (show (primeGen 100))
