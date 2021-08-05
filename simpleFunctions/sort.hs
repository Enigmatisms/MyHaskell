-- 归并排序

mergeSerializedArray :: (Ord a) => [a] -> [a] -> Int -> Int -> [a]
mergeSerializedArray arr1 arr2 p1 p2
    | (all_smaller && p1_bigger)        = (arr2 !! p2):(mergeSerializedArray arr1 arr2 p1 (p2 + 1))
    | (all_smaller && (not p1_bigger))  = (arr1 !! p1):(mergeSerializedArray arr1 arr2 (p1 + 1) p2)
    | (not_finished1)                   = (arr1 !! p1):(mergeSerializedArray arr1 arr2 (p1 + 1) p2)
    | (not_finished2)                   = (arr2 !! p2):(mergeSerializedArray arr1 arr2 p1 (p2 + 1))
    | otherwise                         = []
    where not_finished1 = (p1 < (length arr1))
          not_finished2 = (p2 < (length arr2))
          all_smaller = (not_finished1 && not_finished2)
          p1_bigger = ((arr1 !! p1) >= (arr2 !! p2))

sortTwoElem :: (Ord a) => Int -> Int -> [a] -> [a]
sortTwoElem s e array
    | (s_val >= e_val)  = [e_val, s_val]
    | otherwise         = [s_val, e_val]
    where s_val = (array !! s)
          e_val = (array !! e)

sortArray :: (Ord a) => Int -> Int -> [a] -> [a]
sortArray s e array
    | (s == e)      = [(array !! s)]
    | (e == s + 1)  = (sortTwoElem s e array) 
    | otherwise     = (mergeSerializedArray (sortArray s m array) (sortArray (m + 1) e array) 0 0)
    where m = ((s + e) `div` 2)

arr = [1, 5, 9, 3, 2, 7, 10, 4, 2, 6, 0, 3, 8]
len = (length arr)
main = putStrLn(show (sortArray 0 (len - 1) arr))