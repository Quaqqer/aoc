{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char (chr, ord)

main = do
  input <- map (\[a, _space, b] -> (a, translateB b)) . lines <$> getContents

  putStrLn $ "Part a: " ++ show (sum (map scoreA input))
  putStrLn $ "Part b: " ++ show (sum (map scoreB input))

-- | X -> A, Y -> B, Z -> C
translateB :: Char -> Char
translateB c = chr (ord c - 23)

scoreA :: (Char, Char) -> Int
scoreA (a, b) =
  let -- A -> 1, B -> 2, C -> 3
      moveScore = ord b - 65 + 1
      resultScore =
        if
            -- If equal it's a tie
            | a == b -> 3
            -- If b is the next move from a it's a win
            | ord b - 65 == (ord a - 65 + 1) `mod` 3 -> 6
            -- Otherwise it's a lose
            | otherwise -> 0
   in moveScore + resultScore

scoreB :: (Char, Char) -> Int
scoreB (a, b) =
  let (resultScore, moveDelta) = case b of
        -- To lose, use the next next move
        'A' -> (0, 2)
        -- To tie use the same move
        'B' -> (3, 0)
        -- To win use the next move
        'C' -> (6, 1)
        _ -> error $ "Unexpected char '" ++ [b] ++ "'"
      -- Score the move
      moveScore = ((ord a - 65 + moveDelta) `mod` 3) + 1
   in resultScore + moveScore
