module BnfRandomiser.Main (load, gen, run) where

import BnfRandomiser.Data
import Data.IORef

import Data.Map.Lazy (Map, (!), (!?))
import qualified Data.Map.Lazy as Map

ruleset :: IO (IORef Ruleset)
ruleset = newIORef Map.empty

-- loads a bnf file into memory
-- arguments: filename
load :: String -> IO ()
load = undefined

-- generates a value from loaded file
-- arguments: smybol to generate
gen :: Symbol -> IO String
gen = undefined

-- runs a bnf including `generate` commands without saving to memory
-- arguments: filename
run :: String -> IO ()
run = undefined
