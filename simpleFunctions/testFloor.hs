
{-
    大概明白这个意思了，Num / Integral / RealFrac都是抽象的类
    Int Float Double 则是具体的类型
    所以类型约束作用于抽象类，需要使用类似C语言中的例化，如下可行

    myFloor :: (Integral b) => Float -> b
    myFloor x = floor x

    floor这个函数接收一个RealFrac, RealFrac 包含 Float Double
    但如果是Num（包含Int什么的），无法使用floor

    所以，需要转换：
    myFloor :: (Integral b) => Int -> b
    myFloor x = floor (fromIntegral x)
-}
myFloor :: (Integral b) => Int -> b
myFloor x = floor (fromIntegral x)
main = putStrLn (show (myFloor 1))