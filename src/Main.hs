{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Applicative
import Data.Char
import Data.Maybe
import FUtil
import System.IO

data L = L {unL :: [(String, Either String L)]}
  deriving Show

pp :: Int -> L -> [String]
pp plyDone a = l:lab:ls where
  (l:ls, lab) = ppLinesAndTopLabel plyDone a

ppLinesAndTopLabel :: Int -> L -> ([String], String)
ppLinesAndTopLabel plyDone (L m) = (ls ++ restLs, label)
  where
  (k, v):rest = m
  k' = if plyDone `mod` 2 == 0
    then show (plyDone `div` 2 + 1) ++ " " ++ k
    else k
  (ls, label) = case v of
    Left lab -> ([k], lab)
    Right a -> (zipWith (\ l1 l2 -> l1 ++ " " ++ l2)
      (k' : repeat (replicate (length k') ' '))
      ls, lab)
      where
      (ls, lab) = ppLinesAndTopLabel (plyDone + 1) a
  restLs =
    if null rest
      then []
      else pp plyDone $ L rest

merge :: String -> String -> String
merge prev new = take n prev ++ drop n new where
  n = length (takeWhile (== ' ') new)

deepInsert :: [String] -> String -> L -> L
deepInsert (k:[]) v (L m) = L $ lInsert k (Left v) m
deepInsert (k:ks) v (L m) = L $ lInsert k (Right . deepInsert ks v . L .
  fromMaybe [] . fmap (unL . fromRight) $ lookup k m) m

addFullLine :: L -> String -> String -> L
addFullLine m l name = deepInsert (filter ((> 1) . length) $ words l) name m

lInsert :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
lInsert k v [] = [(k, v)]
lInsert k v ((k1, v1):rest) =
  if k == k1
    then (k, v):rest
    else (k1, v1) : lInsert k v rest

addLines :: String -> L -> [(String, String)] -> L
addLines _ m [] = m
addLines prevLine m ((l, name):rest) =
  addLines prevLine' (addFullLine m prevLine' name) rest
  where prevLine' = merge prevLine l

main :: IO ()
main = do
  t <- addLines "" (L []) .  map (\ [a, b] -> (a, dropWhile isSpace b)) .
    splitN 2 . filter (not . null) . lines <$> readFile "games/lines"
  putStr . unlines $ pp 0 t

