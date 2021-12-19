import Data.List

_splitOn :: String -> String -> String -> [String]
_splitOn split text cur
  | length text < splen = [cur ++ text]
  | take splen text == split = cur : _splitOn split (drop splen text) ""
  | otherwise = _splitOn split (drop 1 text) (cur ++ take 1 text)
  where
    splen = length split

splitOn :: String -> String -> [String]
splitOn text split = _splitOn text split ""

rotationMatrix :: [Int] -> [Int] -> [[Int]]
rotationMatrix positions sizes = [row1, row2, row3]
  where
    row1 = replicate p1 0 ++ [v1] ++ replicate (3 - p1 - 1) 0
    row2 = replicate p2 0 ++ [v2] ++ replicate (3 - p2 - 1) 0
    row3 = replicate p3 0 ++ [v3] ++ replicate (3 - p3 - 1) 0
    [v1, v2, v3] = sizes
    [p1, p2, p3] = positions

rotationMatrices = map (uncurry rotationMatrix) $ [(x,y) | x <- perms,y <- size ]
  where
    perms = permutations [0, 1, 2]
    size = sequence [[-1, 1], [-1, 1], [-1, 1]]

main = do
  contents <- getContents
  let scanners = map (tail . lines) $ splitOn "\n\n" contents
  print rotationMatrices

  print scanners
  print rotationMatrices
