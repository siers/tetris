{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tetris (
  -- Game state modifiers
  initGame,
  timeStep,
  shift,
  hardDrop,
  rotate,
  rotateBlock,
  rotateBlockRaw,
  clearFullRows,
  freezeBlock,
  initNextBlock,
  initBlock,
  -- Game state handlers
  execTetris,
  evalTetris,
  runTetris,
  -- Game state queries
  isGameOver,
  isFree,
  isValidBlockPosition,
  hardDroppedBlock,
  coords,
  -- Types
  Block (..),
  Coord,
  Direction (..),
  BlockRotation (..),
  Game (..),
  Tetrimino (..),
  Translatable (..),
  Board,
  Tetris,
  TetrisT,
  TetrisIO,
  -- Lenses
  block,
  board,
  level,
  nextShape,
  score,
  shape,
  perf,
  rnd,
  -- Constants
  boardHeight,
  boardWidth,
  relCells,
  -- Utils
  shuffle,
)
where

import Control.Applicative ((<|>))
import Control.Lens hiding (Empty, (:<))
import Control.Monad (mfilter, when, (<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State (StateT (..), evalStateT, execStateT, gets)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Linear.V2 (V2 (..), _y)
import System.Random (StdGen, initStdGen, randomR)
import Prelude hiding (Left, Right)

-- Types and instances

-- | Tetris shape types
data Tetrimino = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum)

-- | Coordinates
type Coord = V2 Int

-- | Tetris shape in location context
data Block = Block
  { _shape :: Tetrimino
  -- ^ block type
  , _origin :: Coord
  -- ^ origin
  , _extra :: [Coord]
  -- ^ extraneous cells
  }
  deriving (Eq, Show)

makeLenses ''Block

data Direction = Left | Right | Down
  deriving (Eq, Show)

data BlockRotation = BrRot0 | BrRot90 | BrRot180 | BrRot270
  deriving (Eq, Show, Enum)

{- | Board

If coordinate not present in map, yet in bounds, then it is empty,
otherwise its value is the type of tetrimino occupying it.
-}
type Board = Map Coord Tetrimino

-- | Game state
data Game = Game
  { _level :: Int
  , _block :: Block
  , _nextShape :: Tetrimino
  , _nextShapeBag :: Seq.Seq Tetrimino
  , _rowClears :: Seq.Seq Int
  , _score :: Int
  , _board :: Board
  , _perf :: Int -- solver's performance score
  , _rnd :: StdGen
  }
  deriving (Eq, Show)

makeLenses ''Game

type TetrisT = StateT Game

type Tetris a = forall m. (Monad m) => TetrisT m a

type TetrisIO a = forall m. (MonadIO m) => TetrisT m a

evalTetris :: Tetris a -> Game -> a
evalTetris m = runIdentity . evalStateT m

execTetris :: Tetris a -> Game -> Game
execTetris m = runIdentity . execStateT m

runTetris :: Tetris a -> Game -> (a, Game)
runTetris m = runIdentity . runStateT m

-- Translate class for direct translations, without concern for boundaries
-- 'shift' concerns safe translations with boundaries
class Translatable s where
  translate :: Direction -> s -> s
  translate = translateBy 1
  translateBy :: Int -> Direction -> s -> s

instance Translatable Coord where
  translateBy n Left (V2 x y) = V2 (x - n) y
  translateBy n Right (V2 x y) = V2 (x + n) y
  translateBy n Down (V2 x y) = V2 x (y - n)

instance Translatable Block where
  translateBy n d b =
    b
      & origin %~ translateBy n d
      & extra %~ fmap (translateBy n d)

instance Semigroup BlockRotation where
  a <> b = toEnum (flip mod 4 $ fromEnum a + fromEnum b)

-- Low level functions on blocks and coordinates

initBlock :: Tetrimino -> Block
initBlock t = Block t startOrigin . fmap (+ startOrigin) . relCells $ t

relCells :: Tetrimino -> [Coord]
relCells I = map (uncurry V2) [(-2, 0), (-1, 0), (1, 0)]
relCells O = map (uncurry V2) [(-1, 0), (-1, -1), (0, -1)]
relCells S = map (uncurry V2) [(-1, -1), (0, -1), (1, 0)]
relCells Z = map (uncurry V2) [(-1, 0), (0, -1), (1, -1)]
relCells L = map (uncurry V2) [(-1, -1), (-1, 0), (1, 0)]
relCells J = map (uncurry V2) [(-1, 0), (1, 0), (1, -1)]
relCells T = map (uncurry V2) [(-1, 0), (0, -1), (1, 0)]

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

-- | Starting block origin
startOrigin :: Coord
startOrigin = V2 6 22

{- | Rotate the block clockwise (or counter-) around origin
*Note*: Strict unsafe rotation not respecting boundaries
Safety can only be assured within Game context
-}
rotateBlockRaw :: BlockRotation -> Block -> Block
rotateBlockRaw br b@(Block s o@(V2 xo yo) cs)
  | s == O = b
  | s == I && br == BrRot0 = b
  | s == I && V2 xo (yo + 1) `elem` cs = rotateWith BrRot270
  | s == I = rotateWith BrRot90
  | otherwise = rotateWith br
 where
  rotate BrRot0 v = v
  rotate BrRot90 (V2 x y) = V2 y (-x)
  rotate BrRot180 (V2 x y) = V2 (-x) (-y)
  rotate BrRot270 (V2 x y) = V2 (-y) x
  rotateWith br = b & extra %~ fmap ((+ o) . rotate br . subtract o)

-- | Get coordinates of entire block
coords :: Block -> [Coord]
coords b = b ^. origin : b ^. extra

-- Higher level functions on game and board

{- | Facilitates cycling through at least 4 occurences of each shape
before next bag (random permutation of 4*each tetrimino) is created. If input is empty,
generates new bag, otherwise just unshifts the first value and returns pair.
-}
bagFourTetriminoEach :: Seq.Seq Tetrimino -> Tetris (Tetrimino, Seq.Seq Tetrimino)
bagFourTetriminoEach (t :<| ts) = pure (t, ts)
bagFourTetriminoEach Empty =
  bagFourTetriminoEach <=< shuffle . Seq.fromList . take 28 $ cycle [I ..]

-- | Initialize a game with a given level
initGame :: Int -> Maybe StdGen -> IO Game
initGame lvl rndGen = do
  rnd <- flip fromMaybe rndGen <$> initStdGen
  pure . execTetris (nextBlock >> nextBlock) $
    Game
      { _level = lvl
      , _block = initBlock I
      , _nextShape = I
      , _nextShapeBag = Seq.empty
      , _score = 0
      , _rowClears = mempty
      , _board = mempty
      , _perf = 0
      , _rnd = rnd
      }

isGameOver :: Game -> Bool
isGameOver g = blockStopped g && g ^. (block . origin) == startOrigin

-- | The main game execution, this is executed at each discrete time step
timeStep :: (MonadIO m) => TetrisT m ()
timeStep = do
  gets blockStopped >>= \case
    False -> gravitate
    True -> do
      freezeBlock
      n <- clearFullRows
      addToRowClears n
      updateScore
      updatePerf
      nextBlock

-- | Gravitate current block, i.e. shift down
gravitate :: Tetris ()
gravitate = shift 1 Down

-- | If necessary: clear full rows and return the count
clearFullRows :: Tetris Int
clearFullRows = do
  brd <- use board
  let rowSize r = length $ M.filterWithKey (\(V2 _ y) _ -> r == y) brd
      fullRows = filter (\r -> boardWidth == rowSize r) [1 .. boardHeight]
  -- Clear cells in full rows
  modifying board $ M.filterWithKey $ \(V2 _ y) _ -> y `notElem` fullRows
  -- Shift cells above full rows
  modifying board $ M.mapKeysMonotonic $ over _y $ \y ->
    y - length (filter (< y) fullRows)
  return $ length fullRows

-- | Empties row on 0, otherwise appends value (just keeps consecutive information)
addToRowClears :: Int -> Tetris ()
addToRowClears 0 = rowClears .= mempty
addToRowClears n = rowClears %= (|> n)

{- | This updates game points with respect to the current
_rowClears value (thus should only be used ONCE per step)

Note I'm keeping rowClears as a sequence in case I want to award
more points for back to back clears, right now the scoring is more simple,
but you do get more points for more rows cleared at once.
-}
updateScore :: Tetris ()
updateScore = do
  multiplier <- (1 +) <$> use level
  clears <- latestOrZero <$> use rowClears
  let newPoints = multiplier * points clears
  score %= (+ newPoints)
 where
  -- Translate row clears to points
  points 0 = 0
  points 1 = 40
  points 2 = 100
  points 3 = 300
  points _ = 800
  -- \| Get last value of sequence or 0 if empty
  latestOrZero :: Seq.Seq Int -> Int
  latestOrZero Empty = 0
  latestOrZero (_ :|> n) = n

updatePerf :: Tetris ()
updatePerf = do
  (perf .=) . M.size =<< use board

-- | Allows wallkicks: http://tetris.wikia.com/wiki/TGM_rotation
rotateBlock :: BlockRotation -> Board -> Block -> Maybe Block
rotateBlock br brd blk = do
  foldr (<|>) Nothing $
    mfilter (isValidBlockPosition brd) . pure . ($ blk)
      <$> [ rotateBlockRaw br
          , rotateBlockRaw br . translate Left
          , rotateBlockRaw br . translate Right
          ]

rotate :: BlockRotation -> Tetris ()
rotate br = do
  (brd, blk) <- (,) <$> use board <*> use block
  mapM_ (assign block) (rotateBlock br brd blk)

blockStopped :: Game -> Bool
blockStopped g = isStopped (g ^. board) (g ^. block)

-- | Check if a block on a board is stopped from further gravitation
isStopped :: Board -> Block -> Bool
isStopped brd = any stopped . coords
 where
  stopped = (||) <$> atBottom <*> (`M.member` brd) . translate Down
  atBottom = (== 1) . view _y

hardDrop :: Tetris ()
hardDrop = hardDroppedBlock >>= assign block

hardDroppedBlock :: Tetris Block
hardDroppedBlock = do
  boardCoords <- M.keys <$> use board
  blockCoords <- coords <$> use block
  let diffs =
        [ y - yo
        | (V2 xo yo) <- boardCoords
        , (V2 x y) <- blockCoords
        , xo == x
        , yo < y
        ]
      minY = minimum $ view _y <$> blockCoords
      dist = minimum $ subtract 1 <$> (minY : diffs)
  translateBy dist Down <$> use block

-- | Freeze current block
freezeBlock :: Tetris ()
freezeBlock = do
  blk <- use block
  modifying board $ M.union $ M.fromList [(c, _shape blk) | c <- coords blk]

initNextBlock :: Tetris ()
initNextBlock = use nextShape >>= \s -> block .= initBlock s

-- | Replace block with next block
nextBlock :: Tetris ()
nextBlock = do
  initNextBlock
  bag <- use nextShapeBag
  (t, ts) <- bagFourTetriminoEach bag
  nextShape .= t
  nextShapeBag .= ts

-- | Try to shift current block; if shifting not possible, leave block where it is
shift :: Int -> Direction -> Tetris ()
shift amount dir = do
  brd <- use board
  blk <- use block
  let candidate = translateBy amount dir blk
  when (isValidBlockPosition brd candidate) $
    block .= candidate

-- | Check if coordinate is already occupied or free in board
isFree :: Board -> Coord -> Bool
isFree = flip M.notMember

-- | Check if coordinate is in or out of bounds
isInBounds :: Coord -> Bool
isInBounds (V2 x y) = 1 <= x && x <= boardWidth && 1 <= y

-- | Checks if block's potential new location is valid
isValidBlockPosition :: Board -> Block -> Bool
isValidBlockPosition brd = all validCoord . coords
 where
  validCoord = (&&) <$> isFree brd <*> isInBounds

-- General utilities

-- | Shuffle a sequence (random permutation)
shuffle :: Seq.Seq a -> Tetris (Seq.Seq a)
shuffle xs
  | null xs = pure mempty
  | otherwise = do
      (cut, rnd') <- randomR (0, length xs - 1) <$> use rnd
      rnd .= rnd'
      case Seq.splitAt cut xs of
        (left, y :<| ys) -> fmap (y <|) (shuffle $ left >< ys)
        _ -> error "impossible"
