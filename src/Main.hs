module Main where

import Control.Applicative
import FUtil
import System.IO
import qualified Data.Map as M

type L = M.Map String [L]

addLines :: L -> [(String, String)] -> L
addLines m [] = m
addLines m [l:rest] =
  if tail l == 'T'
    then
    else

main :: IO ()
main = do
  t <- addLines M.empty .
    map (\ [a, b] -> (a, b)) . splitN 2 . filter (not . null) . lines <$>
    readFile "games/lines"
  print t

