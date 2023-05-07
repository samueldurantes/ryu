module Main where

import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment (getArgs)

import qualified Data.Map as Map
import qualified Text.Megaparsec.Char.Lexer as L

type Name = String

-- Type : Type
-- Pi : Type

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  | Let Name Expr Expr

instance Show Expr where
  show (Var n)     = n
  show (Lam n e)   = "λ" ++ n ++ " -> " ++ (show e)
  show (App f a)   = "(" ++ (show f) ++ " " ++ (show a) ++ ")"
  show Let{}       = "<let>"

data HExpr
  = HVar Name
  | HAdd HExpr HExpr
  | HLam (HExpr -> HExpr)
  | HApp HExpr HExpr

type Context = Map Name HExpr

-- Parser

type Parser = Parsec Void Name

skipSpace :: Parser ()
skipSpace = L.space
  space1
  (L.skipLineComment "--")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parens :: Parser a -> Parser a
parens p = do
  _ <- char '('
  a <- lexeme p
  _ <- char ')'
  pure a

keywords :: [Name]
keywords = ["let", "@", "λ", "in"]

pVar :: Parser Expr
pVar = do
  s <- some letterChar
  if not $ s `elem` keywords
    then pure $ Var s
    else fail $ "'" ++ s ++ "' is a keyword"

pLam :: Parser Expr
pLam = do
  _ <- char '@' <|> char 'λ'
  n <- lexeme $ some letterChar
  _ <- lexeme $ string "->"
  e <- pExpr
  pure $ Lam n e

pApp :: Parser Expr
pApp = do
  e <- some (lexeme $ pVar <|> parens pExpr)
  pure $ foldl1 App e

pLet :: Parser Expr
pLet = do
  _ <- lexeme $ string "let"
  n <- lexeme $ some letterChar
  _ <- lexeme $ char '='
  b <- lexeme pExpr
  _ <- lexeme $ char ';'
  e <- lexeme pExpr
  pure $ Let n b e

pExpr :: Parser Expr
pExpr = try $ choice
  [ pLam
  , pLet
  , pApp
  , (pVar <|> parens pExpr)
  ]

parseExpr :: Name -> Either String Expr
parseExpr input =
  let
    outputE = parse
      (between skipSpace eof pExpr)
      ""
      input
  in
  case outputE of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output

-- Evaluation

eval :: Context -> Expr -> HExpr
eval ctx = \case
  Var n     -> fromJust $ Map.lookup n ctx
  Lam n e   -> HLam $ \x ->
    eval (Map.insert n x ctx) e
  App f a   ->
    case (eval ctx f, eval ctx a) of
      (HLam f', a') -> f' a'
      (f',        a') -> HApp f' a'
  Let n b e ->
    eval (Map.insert n (eval ctx b) ctx) e
 
quote :: HExpr -> Expr
quote = go 0
  where
    go :: Int -> HExpr -> Expr
    go _   (HVar n)     = Var n
    go idx (HLam f)     = Lam ("x" ++ show idx) (go (idx + 1) (f (HVar ("x" ++ show idx))))
    go idx (HApp f a)   = App (go idx f) (go idx a)

main :: IO ()
main = do
  [path] <- getArgs
  file   <- readFile path
  case parseExpr file of
    Left err -> putStrLn err
    Right rs -> do
      print $ quote $ eval Map.empty rs
