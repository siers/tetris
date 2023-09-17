{-# LANGUAGE RankNTypes #-}

module Solve2 where

import Control.Lens hiding (Empty, (:<))
import Control.Monad (guard, unless)
import Control.Monad.Trans.State (get, gets)
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (minimumBy)
import Linear.V2 (V2 (..))
import Prelude hiding (Left, Right)

import Tetris

type Score = Int -- smallest best
type BlockMove = (Int, BlockRotation, Block)

horizontalNess :: Board -> Int
horizontalNess b =
  let
    coords = [1 .. boardWidth] >>= (\x -> [1 .. boardHeight - 1] >>= (\y -> [V2 x y]))
    up = translateBy (-1) Down
    ups c = iterate up (up c)
    upsTaken c = takeWhile (not . isFree b) (ups c)
    score c@(V2 _ y) =
      (y *) $
        if isFree b c
          then (4 *) . length . take 2 $ upsTaken c
          else y
   in
    sum . map score $ coords

validBlocks :: Board -> Block -> [BlockMove]
validBlocks brd blk@(Block tetrimino _origin@(V2 x _) _) = do
  shifter <- [(-x) .. (boardWidth - x)]
  rotator <- case tetrimino of
    O -> [BrRot0]
    I -> [BrRot0, BrRot90]
    _ -> [BrRot0, BrRot90, BrRot180, BrRot270]
  let blk' = translateBy shifter Right . rotateBlockRaw rotator $ blk
  guard (isValidBlockPosition brd blk')
  return (shifter, rotator, blk)

rotateShift :: BlockMove -> Tetris ()
rotateShift (s, r, _) = rotate r >> shift s Right

scoreBlock :: BlockMove -> Tetris Score
scoreBlock bm = do
  rotateShift bm
  hardDrop
  freezeBlock
  initNextBlock
  _ <- clearFullRows
  horizontalNess <$> use board

moves :: Tetris [(Score, BlockMove, Game)]
moves = do
  (game, brd, blk) <- (,,) <$> get <*> use board <*> use block

  pure . flip map (validBlocks brd blk) $ \blockMove ->
    let (score, _game) = runTetris (scoreBlock blockMove) game
     in (score, blockMove, _game)

pickMove :: Tetris ()
pickMove = do
  moves <- gets $ \game -> do
    (s1, b, g) <- evalTetris moves game
    (s2, _, _) <- evalTetris moves g
    pure ((s2, s1), b)

  unless (null moves) $
    for_ (minimumBy (compare `on` view _1) moves) rotateShift
