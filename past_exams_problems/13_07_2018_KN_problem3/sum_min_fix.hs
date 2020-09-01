addDefault :: a -> [a] -> [a]
addDefault val [] = [val]
addDefault val l  = l

sumMinFix fs xs =
    foldr1 (+)
    (map 
        (\f -> minimum (addDefault 0 [x | x <- xs, f x == x]))
        fs)

main :: IO ()
main = do
    print(sumMinFix [ (1/), exp, \x -> 2*x - 3] [-2, -1, 1, 3])
