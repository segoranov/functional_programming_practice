-- a)
totalMin fs =
  fst
    ( foldl1
        (\acc fn_with_value_pair -> if snd acc <= snd fn_with_value_pair then acc else fn_with_value_pair)
        (map (\f -> (f, f 0)) fs)
    )

main = do
  print (totalMin [(\x -> x + 2), (\x -> x - 100), (\x -> x + 3)] $ 100) -- tests a), prints 0 correctly.