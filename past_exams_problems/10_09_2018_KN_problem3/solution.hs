selectList :: Foldable t => t a -> t a -> t a
selectList l1 l2 = if length l1 >= length l2 then l1 else l2

sumMaxRoots :: (Eq a1, Num a2, Num a1) => (a2 -> a1) -> [[a2]] -> a2
sumMaxRoots f xss =
    sum
        (foldl
            selectList
            []
            (map (\xs -> [x | x <- xs, f x == 0]) xss))

main = do
    print (sumMaxRoots (\x -> x^3 - x) [ [1, 2, 3], [-1, 0, 5], [1, 4, -1] ]) -- prints -1
