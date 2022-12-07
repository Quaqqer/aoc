{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Debug.Trace
import Text.RawString.QQ
import qualified Text.Regex.TDFA as Re

data FSNode
  = Dir {files :: Map.Map String FSNode}
  | File Int
  deriving (Show)

data Command
  = Ls [LsNode]
  | Cd String
  deriving (Show)

data LsNode
  = LsDir {name :: String}
  | LsFile {name :: String, size :: Int}
  deriving (Show)

main = do
  input <- getContents

  let commands = parseCommands (tail $ lines input)

  let initFs = Dir Map.empty

  let (fs, _) = foldl executeCommand (initFs, []) commands

  let (usedSpace, answerA) = partA fs

  putStrLn $ "Part a: " ++ show answerA

  let diskSpace = 70000000
  let requiredFree = 30000000
  let available = diskSpace - usedSpace
  let missing = requiredFree - available

  putStrLn $ "Part b: " ++ show (fromJust $ snd $ partB missing fs)

  return ()

partA :: FSNode -> (Int, Int)
partA (File size) = (size, 0)
partA (Dir {files}) =
  let (sizes, ns) = map partA (Map.elems files) & unzip
      size = sum sizes
   in (size, sum ns + if size <= 100000 then size else 0)

partB :: Int -> FSNode -> (Int, Maybe Int)
partB _ (File size) = (size, Nothing)
partB missing (Dir {files}) =
  let (sizes, childRemovalSizes) = map (partB missing) (Map.elems files) & unzip
      size = sum sizes
      thisRemovalSize = if size >= missing then Just size else Nothing
      candidates = catMaybes (thisRemovalSize : childRemovalSizes)
      bestRemovalSize = if null candidates then Nothing else Just $ minimum candidates
   in (size, bestRemovalSize)

insertNode :: FSNode -> [String] -> FSNode -> FSNode
insertNode into@(Dir {files}) [name] node = into {files = Map.insert name node files}
insertNode into@(Dir {files}) (head : tail) node =
  into {files = Map.update (\dir -> Just $ insertNode dir tail node) head files}
insertNode into path node = error ""

executeCommand :: (FSNode, [String]) -> Command -> (FSNode, [String])
executeCommand (root, cwd) (Ls lsNodes) =
  let newRoot =
        foldl
          ( \root lsNode ->
              let (fsNode, name) = lsToFsNode lsNode
               in insertNode root (cwd ++ [name]) fsNode
          )
          root
          lsNodes
   in (newRoot, cwd)
executeCommand (root, cwd) (Cd dir) =
  ( root,
    if dir == ".."
      then init cwd
      else cwd ++ [dir]
  )

lsToFsNode :: LsNode -> (FSNode, String)
lsToFsNode (LsDir name) = (Dir {files = Map.empty}, name)
lsToFsNode (LsFile name size) = (File size, name)

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands lines =
  let firstLine = head lines
      cdRegex :: (String, String, String, [String])
      cdRegex = firstLine Re.=~ "\\$ cd (.*)"
      (_, cdMatch, _, [cdDir]) = cdRegex

      lsRegex :: Bool
      lsRegex = firstLine Re.=~ [r|\$ ls|]

      ls :: (Command, [String])
      ls = parseLsNodes (tail lines)

      cd = (Cd cdDir, tail lines)

      (cmd, linesTail) = if lsRegex then ls else cd
   in cmd : parseCommands linesTail

parseLsNodes :: [String] -> (Command, [String])
parseLsNodes lines =
  let (lsLines, tail) = span (\line -> head line /= '$') lines
   in (Ls (map lsLineToNode lsLines), tail)
  where
    lsLineToNode :: String -> LsNode
    lsLineToNode line =
      let dirRegex :: (String, String, String, [String])
          dirRegex = line Re.=~ [r|dir (.*)|]
          (_, dirMatch, _, dirSubmatches) = dirRegex
          [dirName] = dirSubmatches

          fileRegex :: (String, String, String, [String])
          fileRegex = line Re.=~ [r|([0-9]+) (.*)|]
          (_, fileMatch, _, fileSubmatches) = fileRegex
          [fileSize, fileName] = fileSubmatches
       in if dirMatch /= ""
            then LsDir dirName
            else LsFile fileName (read fileSize)
