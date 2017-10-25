module Main (main) where

import Control.Exception (evaluate)
import Data.Foldable     (forM_)
import DeleteLargest
import System.Timeout

main :: IO ()
main = forM_ deletes $ \(name, f) -> do
  res <- timeout 100000 $ evaluate $ length $ take 5 $ f [1..]
  putStrLn $ name ++ ": " ++ maybe "not lazy" (const "lazy") res
