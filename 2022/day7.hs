{-# LANGUAGE QuasiQuotes #-}

import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Map as Map
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

  print $ head commands

  let fs = Dir Map.empty

  let newFs = foldl executeCommand (fs, "/") commands

  print newFs

  return ()

insertNode :: FSNode -> String -> FSNode -> FSNode
insertNode root path node =
  let (name, rest) = span (/= '/') (tail path)
   in (if rest == ""
        then root {files = Map.insert name node (files root)}
        else root {files = Map.update (\dir -> Just $ insertNode dir rest node) name (files root)})

executeCommand :: (FSNode, String) -> Command -> (FSNode, String)
executeCommand (root, cwd) (Ls lsNodes) =
  let newRoot =
        foldl
          ( \root lsNode ->
              let (fsNode, name) = lsToFsNode lsNode
               in insertNode root (cwd ++ name ++ "/") fsNode
          )
          root
          lsNodes
   in (newRoot, cwd)
executeCommand (root, cwd) (Cd dir) =
  ( root,
    if dir == ".."
      then
        reverse cwd
          & tail
          & dropWhile (/= '/')
          & reverse
      else cwd ++ dir ++ "/"
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
