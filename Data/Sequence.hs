{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Sequence where

import BnfRandomiser.Data.Term
import Data.Maybe (maybeToList, fromMaybe)
import YAMP.Module

data Sequence = Seq [Term] Int -- sequence of tokens + weight
   deriving (Show)

getWeight :: Sequence -> Int
getWeight (Seq _ w) = w

instance (MonadPlus m, Foldable m) => Parse m Char Sequence where
   parser = do
      term <- parser
      mseq <- many (optional hspace >> parser)
      weight <- fromMaybe 1 <$> optional (optional hspace >> weightParser)
      pure $ Seq (term : mseq) weight
      where
      weightParser :: MonadPlus m => Parser m Char Int
      weightParser = char ':' >> optional hspace >> anyInt
