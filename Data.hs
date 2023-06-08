module BnfRandomiser.Data (
   Randomise(..), Ruleset, CommandSet, processFile,
   module BnfRandomiser.Data.File,
   module BnfRandomiser.Data.Expression,
   module BnfRandomiser.Data.Sequence,
   module BnfRandomiser.Data.Term,
   module BnfRandomiser.Data.Symbol
) where

import BnfRandomiser.Data.File
import BnfRandomiser.Data.Expression
import BnfRandomiser.Data.Sequence
import BnfRandomiser.Data.Term
import BnfRandomiser.Data.Symbol

import qualified Data.List as List
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import AbLib.System.Random (pick)
import Data.Map.Strict (Map, (!), (!?))

class Randomise a where
   randomise :: Ruleset -> a -> IO String

instance Randomise Expression where
   randomise mem (Expr opts) = do
      seq <- pick opts
      randomise mem seq

instance Randomise Sequence where
   randomise mem (Seq terms) = fmap concat $ sequence $ map (randomise mem) terms

instance Randomise Term where
   randomise _ (Lit str) = pure str
   randomise mem (Sym ident) = case mem !? ident of
      Nothing -> errorWithoutStackTrace ("Symbol '" ++ ident ++ "' not defined!")
      Just def -> randomise mem def

type Ruleset = Map Symbol Expression
type CommandSet = [(CommandType, Args)]

processFile :: FilePath -> [Line] -> IO (Ruleset, CommandSet)
processFile path lines = do
   let dir = List.dropWhileEnd (\c -> c /= '\\' && c /= '/') path
   lines' <- sequence $ map (processLoad dir) lines
   let (rules, cmds) = List.partition isRule lines'
   let ruleset = Map.fromList $ fmap (\(Rule s e) -> (s,e)) rules
   let cmdset = fmap (\(Command t a) -> (t,a)) cmds
   pure (ruleset, cmdset)
   where
   
   isRule :: Line -> Bool
   isRule (Rule _ _) = True
   isRule _ = False
   
   processLoad :: FilePath -> Line -> IO Line
   processLoad dir (Load sym filename) = do
      fileContents <- readFile (dir ++ filename)
      let expr = Expr $ map (\opt -> Seq [Lit opt]) $ List.lines fileContents
      pure $ Rule sym expr
   processLoad _ x = pure x
