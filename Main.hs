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
   let ruleset = makeRuleset $ parseFile fileContents
   writeIORef memory ruleset

-- generates a value from loaded file
-- arguments: symbol to generate
gen :: Symbol -> IO String
gen symbol = do
   ruleset <- readIORef memory
   randomise ruleset (Sym symbol)

-- runs a bnf including `generate` commands without saving to memory
-- arguments: filename
run :: String -> IO ()
run filename = do
   fileContents <- readFile filename
   let lines = parseFile fileContents
   let ruleset = makeRuleset lines
   let cmds = makeCommandSet lines 
   forM_ cmds $ \cmd -> uncurry execute cmd ruleset

execute :: CommandType -> Args -> Ruleset -> IO ()
execute Generate [symbol] mem = do
   output <- randomise mem (Sym symbol)
   putStrLn output
