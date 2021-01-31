{-# LANGUAGE RecursiveDo #-}
module GUI where

import Control.Monad

import Data.Tuple
import Data.Maybe
import qualified Data.List as List
import qualified Data.Array as Array
import Data.IORef

import System.Random

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified CSP
import Game

import Debug.Trace

cellText :: CSP.Pos -> Game -> String
cellText p g
  | l && not m && f                 = "🏁"
  | f || (w && m)                   = "🚩"
  | l && fromJust (_mistake g) == p = "🧨"
  | l && m                          = "💣"
  | uc && ns > 0                    = show ns
  | otherwise                       = " "
  where
    b = _board g
    l = lost g
    w = _won g

    sq = b `squareAt` p
    m = hasMine sq
    f = isFlagged sq
    uc = uncovered sq

    ns = neighbouringMineCount b p

cellEnabled :: Square -> Bool
cellEnabled Uncovered = False
cellEnabled _         = True

makeCell :: Behavior Game -> CSP.Pos -> UI (Element, Event (Game -> Game))
makeCell bGame p = do
  cell <- UI.button #. "cell"

  -- hack so that we can receive key press events
  on UI.hover cell $ \_ -> UI.setFocus cell

  let
    eUncover = UI.click cell
    eFlag    = filterE ('f' ==) $ UI.keypress cell

  let
    events = concatenate <$> unions [
        uncover p <$ eUncover
      , toggleFlag p <$ eFlag
      ]

  let
    bBoard = _board <$> bGame

    bText = cellText p <$> bGame
    bEnabled = cellEnabled . (`squareAt` p) <$> bBoard

  element cell
    # sink UI.text bText
    # sink UI.enabled bEnabled

  return (cell, events)

newGame :: UI ()
newGame = runFunction $ ffi "location.reload()"

setup :: RandomGen g => IORef g -> Window -> UI ()
setup genRef window = void $ mdo
  g <- liftIO $ atomicModifyIORef genRef (\gen -> swap $ genGame gen 10 (9, 9))

  return window # set title "Minesweeper"
  UI.addStyleSheet window "styles.css"

  let positions = List.groupBy (\(x, _) (x2, _) -> x == x2) $ Array.indices (_board g)
  cells' <- mapM (mapM $ makeCell bGame) positions
  let
    cells = (<$>) (element . fst) <$> cells'
    events = snd <$> concat cells'

  message <- UI.paragraph
  let
    msgText g
      | _won g    = "You win!"
      | lost g    = "You lose."
      | otherwise = ""
    bMsgText = msgText <$> bGame

  element message #
    sink UI.text bMsgText

  btnNewGame <- UI.button #. "newGame"
  on UI.click btnNewGame $ const newGame
  element btnNewGame #
    set UI.text "New game"

  body <- getBody window
  element body #+ [
      grid cells
    , element btnNewGame
    , element message
    ]

  bGame <- accumB g $ concatenate <$> unions events

  return ()

run :: IO ()
run = do
  gen <- getStdGen
  gRef <- newIORef gen
  startGUI defaultConfig { jsStatic = Just "static" } (setup gRef)
