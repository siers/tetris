{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Solve1 where

import Control.Lens hiding (Empty, (:<))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State (StateT (..), get)
import Data.Foldable (for_)
import qualified Data.Map as M
import Data.Sequence (ViewL (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Linear.V2 (V2 (..))
import Prelude hiding (Left, Right)

import Tetris

-- smallest best
type Score = (Int, Int, Int)

type BlockMove = (Int, Int, Block)

viewHeadL :: Seq.Seq a -> Maybe a
viewHeadL s = case Seq.viewl s of
  Seq.EmptyL -> Nothing
  (a :< _) -> Just a

coveredBlocks :: Board -> Int
coveredBlocks b =
  let
    coords = [1 .. boardWidth] >>= (\x -> [1 .. boardHeight - 1] >>= (\y -> [V2 x y]))
    free = isFree b
    ups c = iterate (translateBy (-1) Down) (translateBy (-1) Down c)
    score c = fromEnum (free c) * (3 + length (takeWhile (not . free) (ups c)))
   in
    sum . map score $ coords

towerScore :: Board -> Int
towerScore b =
  let
    ysDown = [boardHeight - 1, boardHeight - 2 .. 1]
    tower x = (boardHeight -) . length $ takeWhile (isFree b . V2 x) ysDown
    towers = map tower [1 .. boardWidth]
   in
    sum $ map (id >>= (*)) towers

scoreBoard :: Tetris Score
scoreBoard = do
  cleared <- clearFullRows
  b <- use board
  pure (-cleared, coveredBlocks b, towerScore b)

validBlocks :: Board -> Block -> [BlockMove]
validBlocks brd blk = do
  shifter <- [-10 .. 10]
  rotator <- [0, 1, 2, 3]
  let blk' = translateBy shifter Right . rotateBlockRaw (toEnum rotator) $ blk
  guard (isValidBlockPosition brd blk')
  return (shifter, rotator, blk)

rotateShift :: BlockMove -> Tetris ()
rotateShift (s, r, _) = rotate (toEnum r) >> shift s Right

scoreBlock :: BlockMove -> Tetris Score
scoreBlock bm = do
  rotateShift bm
  hardDrop
  freezeBlock
  scoreBoard

pickMove :: MonadIO m => TetrisT m ()
pickMove = do
  brd <- use board
  blk <- use block
  validBlocks <- pure (validBlocks brd blk)

  -- liftIO $ hPutStrLn stderr "\x1b[H\x1b[Jthe move:"
  game <- get
  blockMove <- fmap (fmap fst . M.minView . M.fromList) . for validBlocks $ \blockMove -> do
    let (score, _game) = runIdentity $ (runStateT @_ @Identity) (scoreBlock blockMove) game
    -- liftIO $ hPrint stderr ("score", score, blockMove)
    pure (score, blockMove)
  -- liftIO $ hPrint stderr (show blockMove)

  for_ blockMove rotateShift
