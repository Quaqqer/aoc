main :: IO ()
main = do
  contents <- getContents
  let ds = depths contents
  putStrLn $ "Silver: " ++ show (solveA ds)
  putStrLn $ "Gold: " ++ show (solveB ds)

depths :: String -> [Integer]
depths i = map read (lines i)

solveA :: [Integer] -> Integer
solveA (x : y : ds) = (if y > x then 1 else 0) + solveA (y : ds)
solveA _ = 0

solveB :: [Integer] -> Integer
solveB (a : b : c : d : rest) = (if a + b + c < b + c + d then 1 else 0) + solveB (b : c : d : rest)
solveB _ = 0
