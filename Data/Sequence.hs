{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Sequence where

import BnfRandomiser.Data.Term
import BnfRandomiser.Data.Weight
import Data.Maybe (maybeToList, fromMaybe)
import YAMP.Module

data Sequence = Seq [Term] Weight -- sequence of tokens + weight
   deriving (Show)

instance Weighted Sequence where
   getWeight (Seq _ w) = w

instance (MonadPlus m, Foldable m) => Parse m Char Sequence where
   parser = do
      term <- parser
      mseq <- many (optional hspace >> parser)
      weight <- fromMaybe 1 <$> optional (optional hspace >> weightParser)
      pure $ Seq (term : mseq) weight
