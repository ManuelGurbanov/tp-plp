module Expr.Parser (parse, parseEither) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.Expr as AE
import qualified Data.ByteString.UTF8
import Expr

-- | Función parcial para parsear una expresión.
parse :: String -> Expr
parse s =
  case parseEither s of
    Left err -> error $ "Error: " ++ err
    Right expr -> expr

-- | Función total para parsear una expresión.
parseEither :: String -> Either String Expr
parseEither s =
  A.parseOnly
    ( do
        AC.skipSpace
        r <- expr
        AC.skipSpace
        A.endOfInput
        pure r
    )
    $ Data.ByteString.UTF8.fromString s

-- parser :: A.Parser Expr
-- parser = A.choice [rangoLit, constLit]

expr :: A.Parser Expr
expr =
  AE.buildExpressionParser table term
    <?> "Expresión"

term =
  parens expr
    <|> rangoLit
    <|> constLit
    <?> "Expresión simple"

table =
  [ [binary '*' Mult AE.AssocLeft, binary '/' Div AE.AssocLeft],
    [binary '+' Suma AE.AssocLeft, binary '-' Resta AE.AssocLeft]
  ]

binary name fun assoc = AE.Infix (do AC.skipSpace; AC.char name; AC.skipSpace; return fun) assoc

parens :: A.Parser a -> A.Parser a
parens p = do
  AC.char '('
  result <- p
  AC.char ')'
  pure result

number :: A.Parser Float
number = realToFrac <$> AC.scientific

constLit :: A.Parser Expr
constLit = Const <$> number <?> "Constante"

rangoLit :: A.Parser Expr
rangoLit =
  ( do
      lower <- number
      AC.skipSpace
      AC.char '~'
      AC.skipSpace
      upper <- number
      pure $ Rango lower upper
  )
    <?> "Rango"

-- >>> parse "1"
-- >>> parse "-1.7 ~ -0.5"
-- >>> parse "1+2"
-- >>> parse "1 + 2"
-- >>> parse "1 + 2 * 3"
-- >>> parse "1 + 2 + 3"
-- >>> parse "1 + (2 + 3)"
-- >>> parse "1 + 2 ~ 3 + 4"
-- >>> parse "1 - 2 - 3 - 4"
-- >>> parse "(((1 - 2) - 3) - 4)"
-- >>> parse "1 "
-- >>> parse "   1    "
-- Const 1.0
-- Rango (-1.7) (-0.5)
-- Suma (Const 1.0) (Const 2.0)
-- Suma (Const 1.0) (Const 2.0)
-- Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0))
-- Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0)
-- Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0))
-- Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0)
-- Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0)
-- Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0)
-- Const 1.0
-- Const 1.0

-- >>> parse "a"
-- >>> parse "1~"
-- >>> parse "(((1 - 2) - 3) - 4"
-- Error: Expresión > Expresión simple > Constante: Failed reading: takeWhile1
-- Error: endOfInput
-- Error: Expresión > Expresión simple > Constante: Failed reading: takeWhile1
