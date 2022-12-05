import Control.Arrow ((>>>))
import Data.Foldable (Foldable (..))
import Data.Function ((&))
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace

type State = Seq [Char]

main = do
  input <- getContents

  let [crates, instructions] = splitOn "\n\n" input

  let initialState = constructState $ init $ lines crates

  let finalStateA =
        foldl
          (move False)
          initialState
          (parseInstructions instructions)
  let finalStateB =
        foldl
          (move True)
          initialState
          (parseInstructions instructions)

  putStrLn $ "Part a: " ++ toList (last <$> finalStateA)
  putStrLn $ "Part b: " ++ toList (last <$> finalStateB)

constructState :: [String] -> State
constructState crates =
  map (drop 1 >>> everyNth 4) crates
    & transpose
    & map (filter (/= ' ') >>> reverse)
    & Seq.fromList

-- | Get every nth item from a list
everyNth :: Int -> [a] -> [a]
everyNth n [] = []
everyNth n l = head l : everyNth n (drop n l)

move :: Bool -> State -> (Int, Int, Int) -> State
move partB state (n, from, to) =
  let moved = drop (length oldFrom - n) (fromJust $ state Seq.!? from)
      oldFrom = fromJust $ state Seq.!? from
      newFrom = take (length oldFrom - n) oldFrom
   in Seq.update from newFrom state
        & Seq.adjust' (++ (if partB then id else reverse) moved) to

parseInstructions :: String -> [(Int, Int, Int)]
parseInstructions instr =
  map
    ( \line ->
        let ws = words line
         in (read (ws !! 1), read (ws !! 3) - 1, read (ws !! 5) - 1)
    )
    (lines instr)
