module T2Parser where

import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as P

import           Data.List            (intercalate)

import           Language.Dot

type T2Parser = Parsec String ()

t2Lang :: LanguageDef st
t2Lang = emptyDef { P.commentLine = "//"
                  , P.reservedNames = ["TO", "FROM", "START", "CUTPOINT"]
                  , P.caseSensitive = True}

lexer = P.makeTokenParser t2Lang

reserved = P.reserved lexer
charLiteral = P.charLiteral lexer
natural = P.natural lexer
colon = P.colon lexer
semi = P.semi lexer
whiteSpace = P.whiteSpace lexer



t2 :: T2Parser Graph
t2 = do
    whiteSpace
    many (construct "START" <|> construct "CUTPOINT")
    ts <- concat <$> many transition
    eof
    return $ Graph UnstrictGraph DirectedGraph Nothing ts

construct :: String -> T2Parser Integer
construct s = do
    reserved s
    colon
    n <- natural
    semi
    return n


transition :: T2Parser [Statement]
transition = do
    from <- construct "FROM"
    --
    is <- manyTill instruction (lookAhead $ reserved "TO")
    let label = unlines is
    --
    to <- construct "TO"
    return [NodeStatement (NodeId (IntegerId from) Nothing) []
           ,EdgeStatement [ENodeId DirectedEdge (NodeId (IntegerId to) Nothing)] [AttributeSetValue (NameId "label") (StringId label)]
           ,NodeStatement (NodeId (IntegerId to) Nothing) []
           ]

instruction :: T2Parser String
instruction = manyTill anyChar semi
