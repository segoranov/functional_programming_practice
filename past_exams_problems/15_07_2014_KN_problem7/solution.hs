-- a)
totalMin fs =
  fst $
    foldl1
      (\acc pair -> if snd acc <= snd pair then acc else pair)
      . map (\f -> (f, f 0))
      $ fs

-- TODO: Solve b)

main = do
  print (totalMin [(\x -> x + 2), (\x -> x - 100), (\x -> x + 3)] $ 100) -- tests a), prints 0 correctly.