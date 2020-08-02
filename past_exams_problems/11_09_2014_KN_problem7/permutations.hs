import Data.List hiding (permutations)

-- permutations [1,2,3] -> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

putEverywhere :: a -> [a] -> [[a]]
putEverywhere x xs = [insertAt n x xs | n <- [0..length xs]]
    where insertAt n x xs = take n xs ++ [x] ++ drop n xs

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations (x:xs) = foldl (++) [] [putEverywhere x permTail | permTail <- permutations xs]

main = do
    print (permutations [1,2,3])
    -- print (putEverywhere 1 [2,3,4])
