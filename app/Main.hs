module Main where

import Control.Monad (join, mfilter, when)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Foldable (traverse_)
import Options.Applicative
import Scorer
import qualified System.Directory as D
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import Tetris (Game (..))
import Text.Read (readMaybe)
import UI.Game
import UI.PickLevel (pickLevel)

data Opts = Opts
  { hardDrop :: HardDropOpt
  , level :: Maybe Int
  , score :: Bool
  , solver :: Bool
  }

data HardDropOpt = None | AsciiOnly | CustomChars String

opts :: Parser Opts
opts =
  Opts
    <$> hardDropOpt
    <*> optional
      ( option
          auto
          ( long "level"
              <> short 'l'
              <> metavar "LEVEL"
              <> help "Specify level (unspecified results in prompt)"
          )
      )
    <*> switch
      ( long "high-score"
          <> help "Print high score and exit"
      )
    <*> switch
      ( long "solver"
          <> help "Print solver scores and exit"
      )

hardDropOpt :: Parser HardDropOpt
hardDropOpt = noneOpt <|> asciiOpt <|> custOpt
 where
  noneOpt =
    flag'
      None
      ( long "no-preview"
          <> short 'n'
          <> help "Don't show preview cell"
      )
  asciiOpt =
    flag'
      AsciiOnly
      ( long "ascii-only"
          <> short 'a'
          <> help "Use '[]' as hard drop preview cell"
      )
  custOpt =
    CustomChars
      <$> option
        twoChar
        ( long "preview-chars"
            <> short 'p'
            <> metavar "CHARS"
            <> value "◤◢"
            <> showDefaultWith (const "◤◢")
            <> help "Customize two character preview cell"
        )

fullopts :: ParserInfo Opts
fullopts =
  info
    (helper <*> opts)
    ( fullDesc
        <> header "tetris - the iconic game right in your terminal"
    )

twoChar :: ReadM String
twoChar = do
  cs <- str
  if length cs /= 2
    then readerError "Preview must be two characters long"
    else return cs

hdOptStr :: HardDropOpt -> Maybe String
hdOptStr None = Nothing
hdOptStr AsciiOnly = Just "[]"
hdOptStr (CustomChars s) = Just s

main :: IO ()
main = do
  (Opts hd ml hs slvr) <- execParser fullopts -- get CLI opts/args
  when hs (getHighScore >>= printM >> exitSuccess) -- show high score and exit
  when slvr (solveScores >> exitSuccess)

  newUI <-
    pure
      . join
      $ createUI
      <$> maybe pickLevel return ml -- pick level prompt if necessary
      <*> pure (hdOptStr hd) -- hard drop preview flag
  ui <- maybe newUI return =<< fromSaveGame
  ui' <- playGame ui -- play game
  when (uiIsGameOver ui') $ handleEndGame (_score (_game ui')) -- save & print score
  saveGame ui'

fromSaveGame :: IO (Maybe UI)
fromSaveGame = (mfilter (not . uiIsGameOver) . decode . pack =<<) <$> (readResourceStr =<< getSaveGameFile)

handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> newHighScore
    Just hs -> if s <= hs then justShowScore else newHighScore
 where
  justShowScore = putStrLn $ "Your final score: " ++ show s
  newHighScore = do
    putStrLn $ "Congrats! You just got the new highest score: " ++ show s
    saveResource (show s) =<< getLeaderboardFile

saveGame :: UI -> IO ()
saveGame ui = (saveResource . unpack . encode $ ui) =<< getSaveGameFile

printM :: (Show a) => Maybe a -> IO ()
printM = traverse_ print

readResourceStr :: FilePath -> IO (Maybe String)
readResourceStr f = do
  exists <- D.doesFileExist f
  if exists
    then Just <$> readFile f
    else return Nothing

readResource :: (Read a) => FilePath -> IO (Maybe a)
readResource f = (readMaybe =<<) <$> readResourceStr f

getHighScore :: IO (Maybe Int)
getHighScore = readResource =<< getLeaderboardFile

saveResource :: String -> FilePath -> IO ()
saveResource s f = writeFile f s

getResourceFilename :: String -> IO FilePath
getResourceFilename resource = do
  xdg <- D.getXdgDirectory D.XdgData "tetris"
  D.createDirectoryIfMissing True xdg
  return (xdg </> resource)

getLeaderboardFile :: IO FilePath
getLeaderboardFile = getResourceFilename "leaderboard"

getSaveGameFile :: IO FilePath
getSaveGameFile = getResourceFilename "saved.json"
