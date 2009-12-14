{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Arrow
import Control.Applicative
import Data.Char
import Data.Maybe
import FUtil
import System.Console.GetOpt
import System.Environment
import System.IO

data Opts = Opts {
  optN :: Maybe Int}

defOpts = Opts {
  optN = Nothing}

procOpts = [
  Option "n" ["truncate"]
    (ReqArg (\ n opts -> opts {optN = Just $ read n}) "N")
    "Truncate the tree at a depth of N (>= 1) ply."]

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

lTrunc :: Int -> L -> L
lTrunc n (L a) = if n == 1
  then L $ map (second f) a
  else L $ map (second (lTrunc (n - 1) <$>)) a
  where
  f :: Either String L -> Either String L
  f v = case v of
    Left s -> Left s
    Right (L a') -> Left $ concatMap (ff . snd) a'
  ff :: Either String L -> String
  ff v = case v of
    Left s -> s
    Right (L a'') -> concatMap (ff . snd) a''

main :: IO ()
main = do
  args <- getArgs
  let
    (opts, []) = case getOpt Permute procOpts args of
      (o, n, []) -> (foldl (flip id) defOpts o, n)
      (_, _, e) -> error $ concat e
  t <- addLines "" (L []) .  map (\ [a, b] -> (a, dropWhile isSpace b)) .
    splitN 2 . filter (not . null) . lines <$> readFile "games/lines"
  let
    t' = case optN opts of
      Nothing -> t
      Just n -> lTrunc n t
  putStr . unlines $ pp 0 t'

