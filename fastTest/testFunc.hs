-- test func

func :: Int -> Int
func a = 
    if a `mod` 1 > 0 then
        2 * a
    else
        3 * a

main = putStrLn (show (func 2))