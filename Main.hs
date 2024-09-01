module BnfRandomiser.Main (load, gen, run) where

import BnfRandomiser.Data
import GHC.IO
import Data.IORef
import YAMP.Module

import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum)

memory :: IORef Ruleset
memory = unsafePerformIO $ newIORef Map.empty

-- loads a bnf file into memory
load :: FilePath -> IO ()
load path = do
   file <- readFile path
   (ruleset, _) <- processFile path $ parseFile file
   writeIORef memory ruleset

-- generates a value from loaded file
-- arguments: symbol to generate
gen :: Symbol -> IO String
gen symbol = do
   ruleset <- readIORef memory
   randomise ruleset (Sym symbol)

-- runs a bnf including `generate` commands without saving to memory
run :: FilePath -> IO ()
run path = do
   file <- readFile path
   (ruleset, cmdset) <- processFile path $ parseFile file
   forM_ cmdset $ \cmd -> uncurry execute cmd ruleset
   where
   execute :: CommandType -> Args -> Ruleset -> IO ()
   execute Generate [symbol] mem = do
      output <- randomise mem (Sym symbol)
      putStrLn output

-- parses a single rule and generates
eval :: String -> IO String
eval rule = do
   let primarySymbol = takeWhile (\c -> isAlphaNum c || c `elem` "_-") rule
   (mem, _) <- processFile undefined $ parseFile rule
   randomise mem (Sym primarySymbol)
