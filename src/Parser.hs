module Parser
    (parseString, parseFile)
where

import Control.Applicative ((<*),(*>),(<*>),(<$>))
import Text.Parsec
import Text.Parsec.Language (LanguageDef, emptyDef)
import qualified Text.Parsec.Token as Token

import PetriNet (PetriNet,makePetriNet)
import Property

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
                                          "+", "-", "*", "&&", "||", "!"]
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
parens :: Parser u a -> Parser u a
parens     = Token.parens     lexer -- parses p surrounded by parenthesis
natural :: Parser u Integer
natural    = Token.natural    lexer -- parses a natural number
integer :: Parser u Integer
integer    = Token.integer    lexer -- parses an integer
comma :: Parser u String
comma      = Token.comma       lexer -- parses a comma
whiteSpace :: Parser u ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

optionalCommaSep :: Parser u a -> Parser u [a]
optionalCommaSep p = many (p <* optional comma)

singleOrList :: Parser u a -> Parser u [a]
singleOrList p = braces (optionalCommaSep p) <|> (:[]) <$> p

numberOption :: Parser u Integer
numberOption = option 1 (brackets natural)

ident :: Parser u String
ident = (identifier <|> stringLiteral) <?> "identifier"

identList :: Parser u [String]
identList = singleOrList ident

places :: Parser u [String]
places = reserved "places" *> identList

transitions :: Parser u [String]
transitions = reserved "transitions" *> identList

initial :: Parser u [(String,Integer)]
initial = reserved "initial" *> singleOrList (do
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
statement = Places <$> places <|>
            Transitions <$> transitions <|>
            Arcs <$> arcs <|>
            Initial <$> initial

petriNet :: Parser u PetriNet
petriNet = do
            reserved "petri"
            reserved "net"
            name <- option "" ident
            statements <- braces (many statement)
            let (p,t,a,i) = foldl splitStatement ([],[],[],[]) statements
            return $ makePetriNet name p t a i
        where
            splitStatement (ps,ts,as,is) stmnt = case stmnt of
                  Places p -> (p ++ ps,ts,as,is)
                  Transitions t -> (ps,t ++ ts,as,is)
                  Arcs a -> (ps,ts,a ++ as,is)
                  Initial i -> (ps,ts,as,i ++ is)

preFactor :: Parser u Integer
preFactor = (reservedOp "-" *> return (-1)) <|>
            (reservedOp "+" *> return 1)

linAtom :: Integer -> Parser u LinAtom
linAtom fac = ( integer >>= \lhs ->
                option (Const (fac*lhs)) $ Var (fac*lhs) <$> (reservedOp "*" *> ident)
              ) <|> Var fac <$> ident

term :: Parser u Term
term = Term <$> ((:) <$> (option 1 preFactor >>= linAtom) <*>
                         many (preFactor >>= linAtom))

parseOp :: Parser u Op
parseOp = (reservedOp "<" *> return Lt) <|>
          (reservedOp "<=" *> return Le) <|>
          (reservedOp "=" *> return Eq) <|>
          (reservedOp ">" *> return Gt) <|>
          (reservedOp ">=" *> return Ge)

atom :: Parser u Formula
atom = do
        lhs <- term
        op <- parseOp
        rhs <- term
        return (Atom (LinIneq lhs op rhs))

parensForm :: Parser u Formula
parensForm = atom <|> parens formula

negation :: Parser u Formula
negation = (Neg <$> (reservedOp "!" *> negation)) <|> parensForm

conjunction :: Parser u Formula
conjunction = do
        lhs <- negation
        option lhs ((lhs :&:) <$> (reservedOp "&&" *> conjunction))

disjunction :: Parser u Formula
disjunction = do
        lhs <- conjunction
        option lhs ((lhs :|:) <$> (reservedOp "||" *> disjunction))

formula :: Parser u Formula
formula = disjunction

propertyType :: Parser u PropertyType
propertyType =
        (reserved "safety" *> return Safety) <|>
        (reserved "liveness" *> return Liveness)

property :: Parser u Property
property = do
        pt <- propertyType
        reserved "property"
        name <- option "" ident
        form <- braces formula
        return Property { pname=name, ptype=pt, pformula=form }

parseContent :: Parser u (PetriNet,[Property])
parseContent = do
        whiteSpace
        net <- petriNet
        properties <- many property
        eof
        return (net, properties)

parseString :: String -> (PetriNet,[Property])
parseString str =
        case parse parseContent "" str of
            Left e -> error $ show e
            Right r -> r

parseFile :: String -> IO (PetriNet,[Property])
parseFile file =  do
        contents <- readFile file
        case parse parseContent file contents of
            Left e -> print e >> fail "parse error"
            Right r -> return r
