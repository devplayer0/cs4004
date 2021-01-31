module Main where

import System.Random

import qualified CSP
import qualified Game
import qualified GUI

main :: IO ()
main = GUI.run

g = Game.cspGame Game.testBoard
pg = putStrLn . Game.strGame . fst
ms = CSP.moves . snd

ucs :: [CSP.Pos] -> Game.CSPGameState -> Game.CSPGameState
ucs ps g = foldr (\p -> snd . Game.uncoverCSP p) g ps

gen = mkStdGen 12312
--gen = mkStdGen 1337
--gen = mkStdGen 123
(g2, _) = Game.genCSPGame gen 10 (9, 9)
