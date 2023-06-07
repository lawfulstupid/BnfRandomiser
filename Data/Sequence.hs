{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Sequence where

import BnfRandomiser.Data.Term
import Data.Maybe (maybeToList)
import YAMP.Module

data Sequence = Seq [Term] -- sequence of tokens
   deriving (Show)

instance (MonadPlus m, Foldable m) => Parse m Char Sequence where
   parser = do
      term <- parser
      mseq <- many (optional hspace >> parser)
      pure $ Seq (term : mseq)
