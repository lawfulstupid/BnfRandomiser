{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Term where

import BnfRandomiser.Data.Symbol
import YAMP.Module

data Term = Sym Symbol | Lit String | Ref Int | Opt Term | Any Term | Mny Term
   deriving (Show, Eq)

instance (MonadPlus m, Foldable m) => Parse m Char Term where
   parser = do
      nextChar <- peek anyChar
      term <- case nextChar of
         '"' -> Lit <$> literalParser
         '\'' -> Lit <$> literalParser
         '$' -> Ref <$> backRefParser
         _ -> Sym <$> symbolParser
      mod <- peek anyChar |> pure ' '
      case mod of
         '?' -> anyChar >> pure (Opt term)
         '*' -> anyChar >> pure (Any term)
         '+' -> anyChar >> pure (Mny term)
         _ -> pure term

literalParser :: (MonadPlus m, Foldable m) => Parser m Char String
literalParser = aux '"' <|> aux '\'' where
   aux q = do
      char q
      s <- greedy $ many $ charThat (/= q)
      char q
      pure s

backRefParser :: (MonadPlus m, Foldable m) => Parser m Char Int
backRefParser = do
   match '$'
   anyInt

hasBackRef :: Term -> Bool
hasBackRef t = case t of
   Ref _ -> True
   Lit _ -> False
   Sym _ -> False
   Opt r -> hasBackRef r
   Any r -> hasBackRef r
   Mny r -> hasBackRef r
