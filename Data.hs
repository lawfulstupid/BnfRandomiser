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

import AbLib.System.Random (pickWeighted)
import Data.Map.Strict (Map, (!), (!?))

class Randomise a where
   randomise :: Ruleset -> a -> IO String

instance Randomise Expression where
   randomise mem (Expr opts) = do
      seq <- pickWeighted getWeight opts
      randomise mem seq

instance Randomise Sequence where
   randomise mem (Seq terms _) = let
      termsIO = map (\t -> if isBackRef t then pure t else Lit <$> randomise mem t) terms
      in do
         termsGen <- sequence termsIO
         let eval t = case t of { Lit s -> s; Ref n -> eval (termsGen !! (n-1)) }
         pure $ concat $ map eval termsGen

instance Randomise Term where
   randomise _ (Lit str) = pure str
   randomise mem (Sym ident) = case mem !? ident of
      Nothing -> errorWithoutStackTrace ("Symbol '" ++ ident ++ "' not defined!")
      Just def -> randomise mem def
   randomise _ (Ref n) = errorWithoutStackTrace "Cannot randomise a back reference! (developer error)"

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
      let expr = Expr $ map (\opt -> Seq [Lit opt] 1) $ List.lines fileContents
      pure $ Rule sym expr
   processLoad _ x = pure x
