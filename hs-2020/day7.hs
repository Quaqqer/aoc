{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Debug.Trace
import Text.RawString.QQ
import Text.Regex.TDFA as Re

bagRegex = [r|(.*) bags contain |]

contentRegex = [r|([0-9]+) ([^,.]+) bags?|]

main = do
  input <- getContents
  let bagContents =
        Map.fromList $
          map lineToBags (lines input)

  putStrLn $ "Part a: " ++ show (partA bagContents)
  putStrLn $ "Part b: " ++ show (partB bagContents)

lineToBags :: String -> (String, [(Int, String)])
lineToBags line =
  let ("", _ :: String, rest :: String, [bagName :: String]) =
        line Re.=~ bagRegex

      contents = rest Re.=~ contentRegex :: [[String]]
   in ( bagName,
        map
          (\[_, n, bagName] -> (read n, bagName))
          contents
      )

partA :: Map.Map String [(Int, String)] -> Int
partA bagContents =
  sum (map (fromEnum . containsGoldBag) (Map.keys bagContents))
    - 1
  where
    containsGoldBag "shiny gold" = True
    containsGoldBag bag = any (containsGoldBag . snd) (fromJust $ Map.lookup bag bagContents)

partB :: Map.Map String [(Int, String)] -> Int
partB bagContents =
  let bagContentMap =
        Map.mapWithKey
          bagSize
          bagContents

      lookupSize bagName = fromJust (Map.lookup bagName bagContentMap)

      bagSize bagName [] = 1
      bagSize bagName bagContent =
        sum
          (map (\(n, bagName) -> n * lookupSize bagName) bagContent)
          + 1
   in lookupSize "shiny gold" - 1
