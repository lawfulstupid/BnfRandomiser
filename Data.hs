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
import BnfRandomiser.Data.Weight
import AbLib.Data.Extended

import qualified Data.List as List
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import System.Random (randomIO)
import AbLib.System.Random (pick, pickWeighted)
import Data.Map.Strict (Map, (!), (!?))
import Control.Monad (liftM2)
import Data.Maybe

class Randomise a where
   randomise :: Ruleset -> a -> IO String

instance Randomise Expression where
   randomise mem (Expr opts) = do
      let weightFn = if all isFinite $ map getWeight opts then getWeight else getWeightInfinite
      seq <- pickWeighted weightFn opts
      randomise mem seq

instance Randomise Sequence where
   randomise mem (Seq terms _) = do
      maybeLits <- sequence $ map (randomiseTerm mem []) terms
      let terms' = map (\i -> maybe (terms !! i) Lit (maybeLits !! i)) [0 .. length terms - 1]
      let justList = map (maybe undefined id)
      let lits = justList maybeLits
      termsGen <- justList <$> (sequence $ map (randomiseTerm mem lits) terms')
      pure $ concat termsGen

instance Randomise Term where
   randomise mem term = fromJust <$> randomiseTerm mem [] term

randomiseTerm :: Ruleset -> [String] -> Term -> IO (Maybe String)
randomiseTerm mem lits term = case term of
   Lit str -> pure (Just str)
   Sym sym -> case mem !? sym of
      Nothing -> errorWithoutStackTrace ("Symbol '" ++ sym ++ "' not defined!")
      Just def -> Just <$> randomise mem def
   Ref n -> pure $ if null lits then Nothing else Just (lits !! (n-1))
   Opt t -> do
      skip <- randomIO
      if skip then pure (Just "") else randomiseTerm mem lits t
   Any t -> randomiseTerm mem lits (Opt $ Mny t)
   Mny t -> do
      head <- randomiseTerm mem lits t
      rest <- randomiseTerm mem lits (Any t)
      pure $ liftM2 (++) head rest

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
