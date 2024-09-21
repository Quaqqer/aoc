main :: IO ()
main = do
  contents <- getContents
  putStrLn $ "Solution part 1: " ++ show (part1 contents)
  putStrLn $ "Solution part 2: " ++ show (part2 contents)

part1 :: String -> Int
part1 s = do
  let ns :: [Int] = map read (lines s)
  let toFuel v = max (v `div` 3 - 2) 0
  sum (map toFuel ns)

part2 :: String -> Int
part2 s = do
  let ns :: [Int] = map read (lines s)
  let toFuel v = do
        let fuel = max (v `div` 3 - 2) 0
        if fuel > 0 then fuel + toFuel fuel else fuel
  sum (map toFuel ns)
