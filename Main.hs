module BnfRandomiser.Main (load, gen, run) where

import BnfRandomiser.Data
import GHC.IO
import Data.IORef
import YAMP.Module

import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map

memory :: IORef Ruleset
memory = unsafePerformIO $ newIORef Map.empty

-- loads a bnf file into memory
-- arguments: filename
load :: String -> IO ()
load filename = do
   fileContents <- readFile filename
   let ruleset = parseRuleset fileContents
   writeIORef memory ruleset

-- generates a value from loaded file
-- arguments: symbol to generate
gen :: Symbol -> IO String
gen symbol = do
   ruleset <- readIORef memory
   case ruleset !? symbol of
      Nothing -> errorWithoutStackTrace ("symbol '" ++ symbol ++ "' does not exist")
      Just expr -> randomise ruleset expr

-- runs a bnf including `generate` commands without saving to memory
-- arguments: filename
run :: String -> IO ()
run = undefined
