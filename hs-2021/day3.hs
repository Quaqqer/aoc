import Data.List
import Data.List.Split

mostCommonBits :: [[Int]] -> [Int]
mostCommonBits bins = map (\x -> if (fromIntegral $ sum x :: Float) > (fromIntegral $ length bins :: Float) / 2 then 1 else 0) t
  where
    t = transpose bins

inverseBits = map (\x -> if x == 1 then 0 else 1)

binToInt :: [Int] -> Int
binToInt = foldl (\x -> (+) (2 * x)) 0

main = do
  contents <- getContents
  let bits = map (map read . chunksOf 1) $ lines contents :: [[Int]]
  let mcb = mostCommonBits bits
  let gamma = binToInt mcb
  let epsilon = binToInt $ inverseBits mcb
  let silver = gamma * epsilon
  putStrLn $ "Silver: " ++ show silver
