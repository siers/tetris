{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Solve where

import Control.Lens hiding (Empty, (:<))
import Control.Monad (guard, unless)
import Control.Monad.Trans.State (get, gets)
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe
import Linear.V2 (V2 (..))
import Prelude hiding (Left, Right)

import Tetris

type Score = Int -- smallest best

data BlockMove = BlockMove {bmShift :: Int, bmRot :: BlockRotation, bmBlock :: Block}

horizontalNess :: Board -> Score
horizontalNess b =
  let
    coords = V2 <$> [1 .. boardWidth] <*> [1 .. boardHeight - 1]
    up = translateBy (-1) Down
    badForFree c = map ($ c) [up, up . up, up . up . up, up . translate Left, up . translate Right]
    score c@(V2 _ y) =
      (y *) $
        if isFree b c
          then (4 *) . sum . map (fromEnum . not . isFree b) $ badForFree c
          else y
   in
    sum . map score $ coords

validBlockMoves :: Board -> Block -> [BlockMove]
validBlockMoves brd blk@(Block tetrimino _origin@(V2 x _) _) = do
  shifter <- [(-x) .. (boardWidth - x)]
  rotation <- case tetrimino of
    O -> [BrRot0]
    I -> [BrRot0, BrRot90]
    _ -> [BrRot0, BrRot90, BrRot180, BrRot270]
  blk' <- translateBy shifter Right <$> maybeToList (rotateBlock rotation brd blk)
  guard (isValidBlockPosition brd blk')
  return (BlockMove shifter rotation blk')

rotateShift :: BlockMove -> Tetris ()
rotateShift BlockMove{bmShift, bmRot} = rotate bmRot >> shift bmShift Right

scoreBlock :: BlockMove -> Tetris Score
scoreBlock BlockMove{bmBlock = b} = do
  block .= b
  hardDrop
  freezeBlock
  initNextBlock
  _ <- clearFullRows
  horizontalNess <$> use board

moves :: Tetris [(Score, BlockMove, Game)]
moves = do
  (game, brd, blk) <- (,,) <$> get <*> use board <*> use block

  pure . flip map (validBlockMoves brd blk) $ \blockMove ->
    let (score, _game) = runTetris (scoreBlock blockMove) game
     in (score, blockMove, _game)

-- caveat: if called from UI when I is beside a wall, it won't be able to move it
pickMove :: Tetris ()
pickMove = do
  moves <- gets $ \game -> do
    (s1, b, g) <- evalTetris moves game
    (s2, _, _) <- evalTetris moves g
    pure ((s2, s1), b)

  unless (null moves) $
    for_ (minimumBy (compare `on` view _1) moves) rotateShift
