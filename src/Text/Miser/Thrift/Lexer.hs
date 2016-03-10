module Text.Miser.Thrift.Lexer where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data Literal = Literal String deriving (Show)

data Identifier = Identifier String deriving (Show)

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
  where lineComment = L.skipLineComment "//" <|> L.skipLineComment "#"
        blockComment = L.skipBlockComment "/*" "*/"

listSeparatorConsumer :: Parser ()
listSeparatorConsumer = void (symbol ",") <|> void (symbol ";")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

literal :: Parser Literal
literal = do
  lit <- char '"' >> manyTill alphaNumChar (char '"')
  return $ Literal lit

integer :: Parser Integer
integer = lexeme $ L.integer

reservedWord :: String -> Parser ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

reservedWords :: [String]
reservedWords = [ "oneway"
                , "const"
                , "typedef"
                , "enum"
                , "senum"
                , "extends"
                ] ++ includeTypes ++ namespaceScopes ++ baseTypes ++ containerTypes ++ complexTypes

includeTypes :: [String]
includeTypes = [ "include"
               , "cpp_include"
               , "namespace"
               , "void"
               , "throws"
               , "exception"
               , "service"
               ]

namespaceScopes :: [String]
namespaceScopes = [ "*"
                  , "cpp"
                  , "java"
                  , "py"
                  , "perl"
                  , "rb"
                  , "cocoa"
                  , "csharp"
                  ]

baseTypes :: [String]
baseTypes = [ "bool"
            , "byte"
            , "i8"
            , "i16"
            , "i32"
            , "i64"
            , "double"
            , "string"
            , "binary"
            , "slist"
            ]

containerTypes :: [String]
containerTypes = [ "list"
                 , "map"
                 , "set"
                 ]

complexTypes :: [String]
complexTypes = [ "struct"
               , "union"
               ]
                  
identifier :: Parser Identifier
identifier = lexeme (p >>= check)
  where p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '.')
        check x = if x `elem` reservedWords
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return $ Identifier x
