import Criterion.Main
import DeleteLargest

main :: IO ()
main = do
  lists <- mkLists
  defaultMain $ map
    (\(listName, list) -> bgroup listName $ map
      (\(funcName, f) -> bench funcName $ whnf (length . f) list)
      deletes)
    lists
