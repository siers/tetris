{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Scorer where

import Control.Concurrent.Async
import Control.Lens hiding (Empty, (:<))
import Control.Monad
import Control.Monad.Trans.State (execStateT)
import Criterion.Main
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Environment (getEnvironment, withArgs)
import System.Random
import Prelude hiding (Left, Right)

-- import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Solve as S
import qualified Solve1 as S1
import qualified Solve2 as S2
import Tetris
import qualified Tetris as T

type SolverScores = (Int, [(Int, Double)])

-- dbg :: MonadIO m => Game -> m ()
-- dbg g = liftIO (putStrLn ("score: " ++ show (view score g)))

solve :: Maybe Int -> TetrisIO () -> Game -> IO Game
solve stepGuard solver g =
  if isGameOver g || any (< 1) stepGuard
    then return g
    else do
      -- when (isJust (find (\x -> x `mod` 71 == 0) stepGuard)) (dbg g)
      execStateT (solver >> hardDrop >> timeStep) g >>= solve (subtract 1 <$> stepGuard) solver

scoreSolver :: Maybe Int -> TetrisIO () -> IO SolverScores
scoreSolver steps solver = do
  scores <- sort . map calcPerf <$> replicateConcurrently 4 (solve steps solver =<< initGame 0 (Just (mkStdGen 0)))
  return (sum (map fst scores), scores)
 where
  divToFloat = (/) `on` (fromIntegral :: Int -> Double)
  calcPerf :: Game -> (Int, Double)
  calcPerf game =
    let
      perf = game ^. T.perf
      score = game ^. T.score
      roundTo = (/ 100) . fromIntegral @Integer . round . (* 100)
     in
      (score, roundTo . negate . log $ perf `divToFloat` score)

solvers :: [(String, Maybe Int -> IO ())]
solvers =
  [ ("solver3", \n -> print . ("solver",) =<< scoreSolver n S.pickMove)
  , ("solver2", \n -> print . ("solver2",) =<< scoreSolver n S2.pickMove)
  , ("solver1", \n -> print . ("solver1",) =<< scoreSolver n S1.pickMove)
  ]

solveScores :: IO ()
solveScores = do
  envs <- M.fromList <$> getEnvironment
  let
    iterations = read <$> M.lookup "iter" envs
    criterion = M.lookup "criterion" envs

  case criterion of
    Just opts ->
      withArgs (words opts) $
        defaultMainWith defaultConfig . pure . bgroup "tetris" $
          flip map solvers $ \(name, solver) ->
            bench name (whnfIO (solver iterations))
    Nothing -> do
      -- void $ initGame 0 >>= solve (Just 1000000000) S.pickMove
      forM_ solvers $ \(_, solver) -> solver iterations
