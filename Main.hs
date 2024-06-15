-- SPDX-License-Identifier: MPL-2.0

-- hs2048
-- A simple 2048 game written in Haskell
-- Copyright (c) 2024 Yao Zi <ziyao@disroot.org>

{-# Language ForeignFunctionInterface #-}
{-# Language NondecreasingIndentation #-}
module Main where

import Foreign
import Foreign.C.Types

import Control.Monad (forM_, liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.State

import qualified Data.Char as CH
import qualified Data.List as L

import System.Posix.Time (epochTime)

import Debug.Trace

type ChType = CInt      -- should work on most systems

class CursesChar c where
  toChType :: c -> ChType

instance (CursesChar Char) where
  toChType = CInt . fromIntegral . CH.ord . limit
    where limit c
            | CH.isAscii c = c
            | otherwise    = ' '

instance (CursesChar Int8) where
  toChType = CInt . fromIntegral

foreign import ccall "curses.h initscr"
  cInitScr :: IO ()
foreign import ccall "curses.h endwin"
  cEndWin :: IO ()
foreign import ccall "curses.h addch"
  _cAddCh :: ChType -> IO CInt
foreign import ccall "curses.h getch"
  cGetCh :: IO CInt
foreign import ccall "curses.h move"
  _cMove :: CInt -> CInt -> IO CInt
foreign import ccall "curses.h erase"
  _cErase :: IO CInt
foreign import ccall "curses.h raw"
  cRaw :: IO CInt

ignore :: IO a -> IO ()
ignore = fmap (const ())

cAddCh :: CursesChar c => c -> IO ()
cAddCh = ignore . _cAddCh . toChType

cMove :: (Int, Int) -> IO ()
cMove (y, x) = ignore $ (_cMove (fromIntegral y) (fromIntegral x))

cErase :: IO ()
cErase = ignore _cErase

cPrintStr :: String -> IO ()
cPrintStr = flip forM_ cAddCh

cNextLines :: Int -> IO ()
cNextLines n = cPrintStr (replicate n '\n') >> cAddCh '\r'

keyUp = CInt $ fromIntegral $ CH.ord 'k'
keyDown = CInt $ fromIntegral $ CH.ord 'j'
keyLeft = CInt $ fromIntegral $ CH.ord 'h'
keyRight = CInt $ fromIntegral $ CH.ord 'l'
keyQuit = CInt $ fromIntegral $ CH.ord 'q'

type Score = Int

data Game = Game { gBoard :: [[Int]], gScore :: Score, gSeed :: Int }
  deriving Show

type IOGame a = StateT Game IO a

boardSize = 4
cellNums = boardSize ^ 2
cellSize = 5
cellBorderSize = (cellSize - 1) `div` 2

initGame :: Integral n => n -> Game
initGame = Game initBoard 0 . fromIntegral
  where setAll a = map (const a)
        initBoard = setAll (setAll 0 [1 .. boardSize]) [1 .. boardSize]

drawScreen :: Game -> IO ()
drawScreen game = forM_ (gBoard game) drawLine >> drawStatusBar (gScore game)
  where
    drawLine xs = drawCellBorder >> forM_ xs drawCell >> drawCellBorder
      where drawCell x = cPrintStr $ center cellSize (showIf x)
              where center width str = (nSpace ls) ++ str ++ (nSpace rs)
                      where nSpace = flip replicate ' '
                            len = length str
                            ls = (width - len) `div` 2
                            rs = width - ls - len
                    showIf x
                      | x == 0 = " "
                      | otherwise = show x
            drawCellBorder = cNextLines cellBorderSize
    drawStatusBar score = cNextLines 2 >> (cPrintStr $ "Score: " ++ (show score))

updateWith :: (a -> a) -> Int -> [a] -> [a]
updateWith _ _ [] = []
updateWith f 0 (x:xs) = (f x) : xs
updateWith f n (x:xs) = x : (updateWith f (n - 1) xs)

traceShowLabel x id = trace (x ++ (show id)) id

getRandom :: IOGame Int
getRandom = do
  game <- get
  let seed' = ((gSeed game) * 1103515245 + 12345) `mod` (2 ^ 31)
  put $ game { gSeed = seed' }
  return $ seed' `div` 65536

putNewCell :: IOGame ()
putNewCell = do
  board <- get >>= return . gBoard
  let frees = filter ((== 0) . snd) $
        zip [0 .. boardSize ^ 2 - 1] $ concat board
  let nFree = length frees
  newCellIdx <- getRandom >>= return . (`mod` nFree)
  value <- getRandom
  let putValue = if value `mod` 2 == 0 then 2 else 4
  let (cellX, cellY) = indexToCell $ fst $ frees !! newCellIdx
  let boards' = updateWith (updateWith (const putValue) cellX) cellY board
  modify $ \game -> game { gBoard = boards' }
  where indexToCell idx = (idx `mod` boardSize, idx `div` boardSize)

moveBoard :: [[Int]] -> (Score, [[Int]])
moveBoard xss =
  let (scores, lines) = unzip $ map merge xss'
  in (sum scores, map fixCells lines)
  where xss' = map (filter (/= 0)) xss
        fixCells xs = xs ++ (replicate (boardSize - (length xs)) 0)
        merge :: [Int] -> (Score, [Int])
        merge [] = (0, [])
        merge [x] = (0, [x])
        merge (x1 : x2 : xs)
          | x1 == x2 = let sum = x1 + x2
                           (s, xs') = merge xs
                       in (sum + s, sum : xs')
          | otherwise = let (s, xs') = merge (x2 : xs)
                        in (s, x1 : xs')

doMove :: CInt -> Game -> Game
doMove dir game = let
  (score, board') = moveBoard $ transform dir $ gBoard game
  in game { gBoard = transform' dir board', gScore = score + (gScore game) }
  where transform dir
         | dir == keyRight = map reverse
         | dir == keyUp    = L.transpose
         | dir == keyDown  = L.transpose . reverse
         | otherwise       = id
        transform' dir
         | dir == keyDown  = reverse . L.transpose
         | otherwise       = transform dir

isValidKey :: CInt -> Bool
isValidKey k = k == keyUp || k == keyDown || k == keyLeft || k == keyRight ||
               k == keyQuit

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM cond m = m >>= (\a -> if cond a then return a else untilM cond m)

getValidKey :: IO CInt
getValidKey = untilM isValidKey cGetCh

data ExitReason = Win | Fail | Quit

gameLoop :: IOGame (Score, ExitReason)
gameLoop = do
  game <- get
  let board' = concat $ gBoard game
  if (length $ filter (== 0) board') == 0
  then return (gScore game, Fail)
  else do

  putNewCell

  liftIO $ do
    cErase
    cMove (0, 0)
  get >>= liftIO . drawScreen

  game <- get
  action <- liftIO getValidKey
  if action == keyQuit
  then return (gScore game, Quit)
  else do
  modify $ doMove action

  game <- get
  if any (== 2048) $ concat $ gBoard game
  then return (gScore game, Win)
  else gameLoop

showGameResult :: (Int, ExitReason) -> IO ()
showGameResult (score, reason) = do
  liftIO cErase
  cPrintStr $ (reasonStr reason) ++ "score " ++ (show score)
  cGetCh >> return ()
  where reasonStr Win  = "You win, "
        reasonStr Fail = "You fail, "
        reasonStr Quit = "Exit, "

main = do
  cInitScr
  cErase
  cRaw
  seed <- epochTime
  (evalStateT gameLoop $ initGame $ rawTimeVal seed) >>= showGameResult
  cEndWin
  where rawTimeVal (CTime v) = v
