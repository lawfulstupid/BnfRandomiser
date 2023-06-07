{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Ruleset where

import BnfRandomiser.Data.Rule
import BnfRandomiser.Data.Expression
import BnfRandomiser.Data.Symbol
import YAMP.Module

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

type Ruleset = Map Symbol Expression

parseRuleset :: String -> Ruleset
parseRuleset = fullParseUsing rulesetParser

rulesetParser :: (MonadPlus m, Foldable m) => Parser m Char Ruleset
rulesetParser = do
   optional whitespace
   rules <- many (parser <* optional whitespace)
   eof
   pure $ Map.fromList $ fmap toTuple rules
