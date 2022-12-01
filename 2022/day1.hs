{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.List.Split

main = do
  input <- getContents
  let elvesSnacks :: [[Int]] = map (map read . lines) (splitOn "\n\n" input)
  let elvesSnacksSum = map sum elvesSnacks

  putStrLn $ "Part a: " ++ show (maximum elvesSnacksSum)
  putStrLn $ "Part b: " ++ (show . sum . take 3 . reverse . sort) elvesSnacksSum
