{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Weight where

import YAMP.Module
import AbLib.Data.Extended

type Weight = Extended Int

class Weighted a where
   getWeight :: a -> Weight
   getWeightInfinite :: a -> Weight
   getWeightInfinite x = if isFinite $ getWeight x then Real 0 else Real 1

weightParser :: MonadPlus m => Parser m Char Weight
weightParser = do
   char ':'
   optional hspace
   (Real <$> anyInt) <|> (match "inf" >> pure PosInf)
