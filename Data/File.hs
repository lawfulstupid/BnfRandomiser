{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.File where

import BnfRandomiser.Data.Expression
import BnfRandomiser.Data.Symbol
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import YAMP.Module

data Line = Rule Symbol Expression | Command CommandType Args
   deriving (Show)

data CommandType = Generate deriving (Show)
type Args = [String]

toRuleTuple :: Line -> Maybe (Symbol, Expression)
toRuleTuple (Rule s e) = Just (s,e)
toRuleTuple _ = Nothing

toCommandTuple :: Line -> Maybe (CommandType, Args)
toCommandTuple (Command t a) = Just (t,a)
toCommandTuple _ = Nothing

instance (MonadPlus m, Foldable m) => Parse m Char Line where
   parser = do
      optional hspace
      c <- peek anyChar
      line <- if c == '!' then cmdParser else ruleParser
      optional hspace
      pure line
      where
      ruleParser = do
         sym <- symbolParser
         optional hspace
         char '='
         optional hspace
         exp <- parser
         pure $ Rule sym exp
      cmdParser = do
         char '!'
         optional hspace
         cmdtype <- parser
         args <- many (hspace >> blackspace)
         pure $ Command cmdtype args

instance MonadPlus m => Parse m Char CommandType where
   parser = match "generate" >> pure Generate

fileParser :: (MonadPlus m, Foldable m) => Parser m Char [Line]
fileParser = do
   optional whitespace
   lines <- many (parser <* optional whitespace)
   eof
   pure lines

parseFile :: String -> [Line]
parseFile = fullParseUsing fileParser


type Ruleset = Map Symbol Expression

makeRuleset :: [Line] -> Ruleset
makeRuleset lines = Map.fromList $ catMaybes $ fmap toRuleTuple lines


type CommandSet = [(CommandType, [String])]

makeCommandSet :: [Line] -> CommandSet
makeCommandSet lines = catMaybes $ fmap toCommandTuple lines
