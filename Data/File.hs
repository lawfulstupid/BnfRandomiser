{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.File where

import BnfRandomiser.Data.Expression
import BnfRandomiser.Data.Symbol
import BnfRandomiser.Data.Term
import Data.Maybe (catMaybes)
import Data.List
import Data.Char (isSpace)
import YAMP.Module

data Line = Rule Symbol Expression | Load Symbol FilePath | Command CommandType Args
   deriving (Show)

data CommandType = Generate deriving (Show)
type Args = [String]


instance (MonadPlus m, Foldable m) => Parse m Char Line where
   parser = do
      optional hspace
      c <- peek anyChar
      line <- if c == '!' then cmdParser else ruleParser <|> loadParser
      optional hspace
      optional commentParser
      pure line
      where
      
      cmdParser = do
         char '!'
         optional hspace
         cmdtype <- parser
         args <- many (hspace >> argParser)
         pure $ Command cmdtype args
      
      argParser = literalParser <|> do
         arg <- anyString
         guard (length arg > 0)
         guard $ not $ isPrefixOf "--" arg
         guard $ not $ any isSpace arg
         pure arg
      
      ruleParser = do
         sym <- symbolParser
         optional hspace
         char '='
         optional hspace
         exp <- parser
         pure $ Rule sym exp
      
      loadParser = do
         sym <- symbolParser
         optional hspace
         string "<-"
         optional hspace
         file <- literalParser
         pure $ Load sym file
      
      commentParser = string "--" >> greedy line

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
