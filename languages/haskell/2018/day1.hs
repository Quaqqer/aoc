import Control.Arrow
import Data.Function ((&))
import qualified Data.Set as S

main :: IO ()
main = do
  contents <- getContents
  solveA contents
  solveB contents

solveA :: String -> IO ()
solveA s = putStrLn $ "Silver: " ++ (s & words & map parseSigned & sum & show)

solveB :: String -> IO ()
solveB s = putStrLn $ "Gold: " ++ show (_solveB 0 (s & words & map parseSigned) 0 S.empty)

_solveB :: Integral i => i -> [i] -> Int -> S.Set i -> i
_solveB cur ops i prevs =
  if cur `S.member` prevs
    then cur
    else _solveB (cur + ops !! (i `mod` length ops)) ops (i + 1) (S.insert cur prevs)

parseSigned :: String -> Integer
parseSigned s = (if head s == '+' then 1 else -1) * read (tail s)
