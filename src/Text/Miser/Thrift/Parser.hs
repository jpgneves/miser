module Text.Miser.Thrift.Parser (parseFile) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Miser.Thrift.Lexer

data Document = Document [Header] [Definition] deriving (Show)

data Definition =
  TypeDef DefinitionType Identifier
  | ConstDef FieldType Identifier ConstValue
  | StructDef Identifier [Field]
  | UnionDef Identifier [Field]
  | EnumDef Identifier [EnumElem]
  | ExceptionDef Identifier [Field]
  | ServiceDef Identifier (Maybe Identifier) [Function]
  deriving (Show)

data DefinitionType =
  BaseType String
  | MapType FieldType FieldType
  | ListType FieldType
  | SetType FieldType
  deriving (Show)

data ConstValue =
  IntConstant Integer
  | DoubleConstant Double
  | ListConstant [ConstValue]
  | MapConstant [(ConstValue, ConstValue)]
  deriving (Show)

data EnumElem = EnumElem Identifier (Maybe Integer) deriving (Show)

data Field = Field (Maybe FieldId) (Maybe FieldQualifier) FieldType Identifier (Maybe ConstValue) deriving (Show)

data FieldId = FieldId Integer deriving (Show)

data FieldQualifier =
  RequiredField
  | OptionalField
  deriving (Show)

data FieldType =
  IdentifierFieldType Identifier
  | DefinitionFieldType DefinitionType
  deriving (Show)

data Function = Function Bool FunctionType Identifier [Field] (Maybe Throws)
  deriving (Show)

data FunctionType =
  FunctionType FieldType
  | VoidFunctionType
  deriving (Show)

data Throws = Throws [Field]
  deriving (Show)

data Header =
  IncludeHeader Literal
  | CppIncludeHeader Literal
  | Namespace NamespaceScope Identifier
  deriving (Show)

data NamespaceScope = NamespaceScope String deriving (Show)

includeHeader :: Parser Header
includeHeader = do
  reservedWord "include"
  lit <- literal
  return $ IncludeHeader lit

cppIncludeHeader :: Parser Header
cppIncludeHeader = do
  reservedWord "cpp_include"
  lit <- literal
  return $ CppIncludeHeader lit

namespaceScope :: Parser NamespaceScope
namespaceScope = do
  scope <- choice $ map (\s -> symbol s) namespaceScopes
  return $ NamespaceScope scope

namespaceHeader :: Parser Header
namespaceHeader = do
  reservedWord "namespace"
  scope <- namespaceScope
  ident <- identifier
  return $ Namespace scope ident

header :: Parser Header
header = includeHeader <|> cppIncludeHeader <|> namespaceHeader

headers :: Parser [Header]
headers = manyTill (spaceConsumer *> header) (try $ lookAhead $ spaceConsumer *> definition)

identifierFieldType :: Parser FieldType
identifierFieldType = do
  ident <- identifier
  return $ IdentifierFieldType ident

definitionFieldType :: Parser FieldType
definitionFieldType = do
  defType <- definitionType
  return $ DefinitionFieldType defType

fieldType :: Parser FieldType
fieldType = (try identifierFieldType) <|> definitionFieldType

keyValueType :: Parser (FieldType, FieldType)
keyValueType = do
  keyType <- fieldType
  void $ symbol ","
  valueType <- fieldType
  return $ (keyType, valueType)

baseType :: Parser DefinitionType
baseType = do
  base <- choice $ map (\s -> try $ symbol s) baseTypes
  return $ BaseType base

mapType :: Parser DefinitionType
mapType = do
  reservedWord "map"
  (keyType, valueType) <- subType keyValueType
  return $ MapType keyType valueType

listType :: Parser DefinitionType
listType = do
  reservedWord "list"
  valueType <- subType fieldType
  return $ ListType valueType

setType :: Parser DefinitionType
setType = do
  reservedWord "set"
  valueType <- subType fieldType
  return $ SetType valueType

definitionType :: Parser DefinitionType
definitionType = baseType <|> mapType <|> listType <|> setType

typeDef :: Parser Definition
typeDef = do
  reservedWord "typedef"
  defType <- definitionType
  ident   <- identifier
  return $ TypeDef defType ident

fieldId :: Parser FieldId
fieldId = do
  fid <- integer
  void $ symbol ":"
  return $ FieldId fid

requiredField :: Parser FieldQualifier
requiredField = reservedWord "required" >> return RequiredField

optionalField :: Parser FieldQualifier
optionalField = reservedWord "optional" >> return OptionalField

fieldQualifier :: Parser FieldQualifier
fieldQualifier = requiredField <|> optionalField

field :: Parser Field
field = do
  fId <- optional fieldId
  fQualifier <- optional fieldQualifier
  fType <- fieldType
  ident <- identifier
  value <- optional $ (void $ symbol "=") >> constValue
  return $ Field fId fQualifier fType ident value

fields :: Parser [Field]
fields = many $ spaceConsumer *> field

structDef :: Parser Definition
structDef = do
  reservedWord "struct"
  ident <- identifier
  fs    <- block fields
  return $ StructDef ident fs

unionDef :: Parser Definition
unionDef = do
  reservedWord "union"
  ident <- identifier
  fs    <- block fields
  return $ UnionDef ident fs

enumElem :: Parser EnumElem
enumElem = do
  ident <- identifier
  index <- optional $ (void $ symbol "=") >> integer
  void $ optional $ listSeparatorConsumer
  return $ EnumElem ident index

enumElems :: Parser [EnumElem]
enumElems = many $ spaceConsumer *> enumElem

enumDef :: Parser Definition
enumDef = do
  reservedWord "enum"
  ident <- identifier
  elems <- block enumElems
  return $ EnumDef ident elems

exceptionDef :: Parser Definition
exceptionDef = do
  reservedWord "exception"
  ident <- identifier
  fs    <- block fields
  return $ ExceptionDef ident fs

voidFunctionType :: Parser FunctionType
voidFunctionType = (symbol "void") >> return VoidFunctionType

fieldFunctionType :: Parser FunctionType
fieldFunctionType = do
  ftype <- fieldType
  return $ FunctionType ftype

functionType :: Parser FunctionType
functionType = voidFunctionType <|> fieldFunctionType

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

throws :: Parser Throws
throws = do
  reservedWord "throws"
  fs <- parens fields
  return $ Throws fs

function :: Parser Function
function = do
  oneway <- option False $ (symbol "oneway") >> return True
  ftype <- functionType
  ident <- identifier
  fs    <- parens fields
  throws <- optional $ throws
  void $ optional $ listSeparatorConsumer
  return $ Function oneway ftype ident fs throws

functions :: Parser [Function]
functions = many $ spaceConsumer *> function

extends :: Parser Identifier
extends = reservedWord "extends" >> identifier

serviceDef :: Parser Definition
serviceDef = do
  reservedWord "service"
  ident <- identifier
  ext <- optional extends
  funs <- block functions
  return $ ServiceDef ident ext funs

intConstant :: Parser ConstValue
intConstant = do
  value <- signedInteger
  return $ IntConstant value

doubleConstant :: Parser ConstValue
doubleConstant = do
  value <- signedDouble
  return $ DoubleConstant value

listConstant :: Parser ConstValue
listConstant = do
  items <- list constValues
  return $ ListConstant items

constKeyValue :: Parser (ConstValue, ConstValue)
constKeyValue = do
  key <- constValue
  void $ symbol ":"
  value <- constValue
  void $ optional $ listSeparatorConsumer
  return $ (key, value)

mapConstant :: Parser ConstValue
mapConstant = do
  kvs <- block $ many $ spaceConsumer *> constKeyValue
  return $ MapConstant kvs

constValue :: Parser ConstValue
constValue = try doubleConstant <|> intConstant <|> listConstant <|> mapConstant

constValues :: Parser [ConstValue]
constValues = many $ spaceConsumer *> separatedConstValue
  where separatedConstValue = do
          value <- constValue
          void $ optional $ listSeparatorConsumer
          return value

constDef :: Parser Definition
constDef = do
  reservedWord "const"
  ftype <- fieldType
  ident <- identifier
  void $ symbol "="
  cval <- constValue
  void $ optional $  listSeparatorConsumer
  return $ ConstDef ftype ident cval

definition :: Parser Definition
definition = try typeDef <|> try structDef <|> unionDef <|> try enumDef <|> exceptionDef <|> serviceDef <|> constDef

definitions :: Parser [Definition]
definitions = many $ spaceConsumer *> definition

document :: Parser Document
document = do
  hs <- headers
  ds <- definitions
  return $ Document hs ds

parseFile path = do
  parseFromFile document path
