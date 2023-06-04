import Data.Map.Lazy (Map, (!), (!?))
import qualified Data.Map.Lazy as Map

import AbLib.System.Random (pick)

module BnfRandomiser.Main (load, gen, run) where

type Memory = Map Ident Definition
data Definition = Def [Sequence] -- disjunctive list of options
data Sequence = Seq [Token] -- sequence of tokens
data Token = Symbol Ident | Raw String
type Ident = String

class Randomise a where
   randomise :: Memory -> a -> IO String

instance Randomise Definition where
   randomise mem (Def opts) = do
      seq <- pick opts
      randomise mem seq

instance Randomise Sequence where
   randomise mem (Seq tkns) = fmap concat $ sequence $ map (randomise mem) tkns

instance Randomise Token where
   randomise _ (Raw str) = pure str
   randomise mem (Symbol ident) = case mem !? ident of
      Nothing -> errorWithoutStackTrace ("Symbol '" ++ ident ++ "' not defined!")
      Just def -> randomise mem def

memory :: IO (IORef Memory)
memory = newIORef Map.empty

-- loads a bnf file into memory
-- arguments: filename
load :: String -> IO ()
load = undefined

-- generates a value from loaded file
-- arguments: smybol to generate
gen :: Ident -> IO String
gen = undefined

-- runs a bnf including `generate` commands without saving to memory
-- arguments: filename
run :: String -> IO ()
run = undefined

negus = Map.fromList
   [ ("michael_negus", Def [Seq [Symbol "michael", Symbol "middle", Symbol "negus"]])
   , ("michael", Def [Seq [Raw "monkle"], Seq [Raw "dendis"], Seq [Raw "spuk"]])
   , ("negus", Def [Seq [Raw "nonkle"], Seq [Raw "dobo"], Seq [Raw "jeppis"]])
   , ("middle", Def [Seq [Raw " "], Seq [Raw " j. "], Seq [Raw " h.w. "]])
   ]

inf = Map.fromList
   [ ("bip", Def [Seq [Raw "."], Seq [Raw "L", Symbol "bip"], Seq [Symbol "bip", Raw "R"]]) ]
