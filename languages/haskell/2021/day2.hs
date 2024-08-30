data Action = Up Int | Down Int | Forward Int

parseAction :: [String] -> Action
parseAction ["forward", amt] = Forward $ read amt
parseAction ["up", amt] = Up $ read amt
parseAction ["down", amt] = Down $ read amt
parseAction _ = error "Could not parse action"

data Position
  = PositionA
      { x :: Int,
        y :: Int
      }
  | PositionB
      { x :: Int,
        y :: Int,
        aim :: Int
      }
  deriving (Show)

move :: Position -> Action -> Position
move position action = case position of
  PositionA x y ->
    PositionA {x = x + dx, y = y + dy}
    where
      dx = case action of
        Forward amt -> amt
        _ -> 0
      dy = case action of
        Up amt -> -amt
        Down amt -> amt
        _ -> 0
  PositionB x y aim ->
    PositionB {x = x + fwd, y = y + fwd * aim, aim = aim + dy}
    where
      fwd = case action of
        Forward amt -> amt
        _ -> 0
      dy = case action of
        Up amt -> -amt
        Down amt -> amt
        _ -> 0

main = do
  contents <- getContents
  let actions = map (parseAction . words) (lines contents)
  let silver_pos = foldl move (PositionA {x = 0, y = 0}) actions
  let silver = x silver_pos * y silver_pos
  let gold_pos = foldl move (PositionB {x = 0, y = 0, aim = 0}) actions
  let gold = x gold_pos * y gold_pos
  putStrLn $ "Silver: " ++ show silver
  putStrLn $ "Gold: " ++ show gold
