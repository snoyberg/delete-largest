import Data.List     (intercalate)
import DeleteLargest
import Weigh         (func, mainWith)

main :: IO ()
main = do
  lists <- mkLists
  mainWith $ sequence_ $ do
    (evalType, toLength) <- [("full", False), ("length", True)]
    (listName, list) <- lists
    (funcName, f) <- deletes
    let name = intercalate "/" [evalType, listName, funcName]
    return $
      if toLength
        then func name (length . f) list
        else func name f list
