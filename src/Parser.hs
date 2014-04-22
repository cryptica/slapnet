module Parser
    (parseString, parseFile)
where

import Control.Applicative ((<*))
import Control.Monad (liftM)
import Text.Parsec
import Text.Parsec.Language (LanguageDef, emptyDef)
import qualified Text.Parsec.Token as Token

import PetriNet (PetriNet,makePetriNet)

type Parser u a = Parsec String u a

languageDef :: LanguageDef u
languageDef =
        emptyDef {
                 Token.commentStart    = "/*",
                 Token.commentEnd      = "*/",
                 Token.commentLine     = "//",
                 Token.identStart      = letter <|> char '_',
                 Token.identLetter     = alphaNum <|> char '_',
                 Token.reservedNames   = [],
                 Token.reservedOpNames = ["->", "<", "<=", "=", ">=", ">",
                                          "+", "-", "*", "&", "|", "!"]
                 }

lexer :: Token.TokenParser u
lexer = Token.makeTokenParser languageDef

identifier :: Parser u String
identifier = Token.identifier lexer -- parses an identifier
stringLiteral :: Parser u String
stringLiteral = Token.stringLiteral lexer -- parses a string literal
reserved :: String -> Parser u ()
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp :: String -> Parser u ()
reservedOp = Token.reservedOp lexer -- parses an operator
brackets :: Parser u a -> Parser u a
brackets   = Token.brackets   lexer -- parses p surrounded by brackets
braces :: Parser u a -> Parser u a
braces     = Token.braces     lexer -- parses p surrounded by braces
natural :: Parser u Integer
natural    = Token.natural    lexer -- parses a natural number
comma :: Parser u String
comma       = Token.comma       lexer -- parses a comma
whiteSpace :: Parser u ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

optionalCommaSep :: Parser u a -> Parser u [a]
optionalCommaSep p = many (p <* optional comma)

singleOrList :: Parser u a -> Parser u [a]
singleOrList p = braces (optionalCommaSep p) <|> liftM (:[]) p

numberOption :: Parser u Integer
numberOption = option 1 (brackets natural)

ident :: Parser u String
ident = (identifier <|> stringLiteral) <?> "identifier"

identList :: Parser u [String]
identList = singleOrList ident

places :: Parser u [String]
places = reserved "places" >> identList

transitions :: Parser u [String]
transitions = reserved "transitions" >> identList

initial :: Parser u [(String,Integer)]
initial = reserved "initial" >> singleOrList (do
            n <- ident
            i <- numberOption
            return (n,i)
          )

arc :: Parser u [(String,String,Integer)]
arc = do
        lhs <- identList
        rhsList <- many1 (do
                reservedOp "->"
                w <- numberOption
                ids <- identList
                return (ids,w)
            )
        return $ fst $ foldl makeArc ([],lhs) rhsList
      where
        makeArc (as,lhs) (rhs,w) = ([(l,r,w) | l <- lhs, r <- rhs] ++ as, rhs)

arcs :: Parser u [(String,String,Integer)]
arcs = do
        reserved "arcs"
        as <- singleOrList arc
        return $ concat as

data Statement = Places [String] | Transitions [String] |
                 Arcs [(String,String,Integer)] | Initial [(String,Integer)]

statement :: Parser u Statement
statement = liftM Places places <|>
            liftM Transitions transitions <|>
            liftM Arcs arcs <|>
            liftM Initial initial

petriNet :: Parser u PetriNet
petriNet = do
            whiteSpace
            reserved "petri"
            reserved "net"
            name <- option "" ident
            statements <- braces (many statement)
            eof
            let (p,t,a,i) = foldl splitStatement ([],[],[],[]) statements
            return $ makePetriNet name p t a i
        where
            splitStatement (ps,ts,as,is) stmnt = case stmnt of
                  Places p -> (p ++ ps,ts,as,is)
                  Transitions t -> (ps,t ++ ts,as,is)
                  Arcs a -> (ps,ts,a ++ as,is)
                  Initial i -> (ps,ts,as,i ++ is)

parseString :: String -> PetriNet
parseString str =
        case parse petriNet "" str of
            Left e -> error $ show e
            Right r -> r

parseFile :: String -> IO PetriNet
parseFile file =  do
        contents <- readFile file
        case parse petriNet file contents of
            Left e -> print e >> fail "parse error"
            Right r -> return r
