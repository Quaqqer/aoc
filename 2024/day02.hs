main = do
  input <- getContents
  putStrLn $ "Answer A: " ++ show (solveA input)
  putStrLn $ "Answer B: " ++ show (solveB input)

safe :: [Int] -> Bool
safe ls = (increasing || decreasing) && changeLT4
  where
    increasing = allPairsAre (<)
    decreasing = allPairsAre (>)
    changeLT4 = allPairsAre (\a b -> abs (a - b) <= 3)
    allPairsAre op = all (uncurry op) (zip ls (tail ls))

solveA :: String -> Int
solveA s = sum (map fromEnum safeLines)
  where
    safeLines = map (safe . map read . words) ls
    ls = lines s

solveB :: String -> Int
solveB s = sum (map fromEnum safeLines)
  where
    safeLines = map (any safe) testLines
    testLines = map (allSkips . map read . words) ls
    ls = lines s

allSkips :: [a] -> [[a]]
allSkips = inner []
  where
    inner agg (a : b) = (agg ++ b) : inner (agg ++ [a]) b
    inner agg [] = []
