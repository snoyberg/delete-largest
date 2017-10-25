module DeleteLargest
  ( deleteAlls
  , deleteFirsts
  , deletes
  , simpleDeleteAll
  , simpleDeleteFirst
  , mkLists
  ) where

import Control.Exception (evaluate)
import Data.DList        (apply, singleton, snoc, toList)
import Data.List         (delete)
import System.Random     (mkStdGen, randoms)

mkLists :: IO [(String, [Int])]
mkLists = do
  let asc = [1..10000]
      desc = reverse asc
      random = take 10000 $ randoms $ mkStdGen 0

  let res =
        [ ("ascending", asc)
        , ("descending", desc)
        , ("random", random)
        ]

  mapM_ (evaluate . sum . snd) res

  return res

deleteAlls :: [(String, [Int] -> [Int])]
deleteAlls =
  [ ("simpleDeleteAll", simpleDeleteAll)
  , ("diffListAll", diffListAll)
  ]

deleteFirsts :: [(String, [Int] -> [Int])]
deleteFirsts =
  [ ("simpleDeleteFirst", simpleDeleteFirst)
  , ("diffListFirst", diffListFirst)
  ]

deletes :: [(String, [Int] -> [Int])]
deletes = deleteAlls ++ deleteFirsts

simpleDeleteAll :: Ord a => [a] -> [a]
simpleDeleteAll [] = []
simpleDeleteAll list = filter (/= maximum list) list

simpleDeleteFirst :: Ord a => [a] -> [a]
simpleDeleteFirst list = delete (maximum list) list

diffListAll :: Ord a => [a] -> [a]
diffListAll [] = []
diffListAll (x0:xs0) =
    start x0 xs0
  where
    start x xs = go x mempty (singleton x) xs

    go _largest ifLargest _ifNot [] = toList ifLargest
    go largest ifLargest ifNot (x:xs) =
      case compare x largest of
        LT -> go largest (ifLargest `snoc` x) (ifNot `snoc` x) xs
        EQ -> go largest ifLargest (ifNot `snoc` x) xs
        GT -> apply ifNot (start x xs)

diffListFirst :: Ord a => [a] -> [a]
diffListFirst [] = []
diffListFirst (x0:xs0) =
    start x0 xs0
  where
    start x xs = go x mempty xs

    go _largest front [] = toList front
    go largest front (x:xs)
      | x <= largest = go largest (front `snoc` x) xs
      | otherwise = largest : apply front (start x xs)
