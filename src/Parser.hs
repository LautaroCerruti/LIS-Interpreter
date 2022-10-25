module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "while", "skip" , "do"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "?"
                        , ":"
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = ternary <|> mathexp

ternary :: Parser (Exp Int)
ternary = try (do bexp <- boolatom
                  reservedOp lis "?"
                  iexp1 <- intexp
                  reservedOp lis ":"
                  iexp2 <- intexp
                  return (ECond bexp iexp1 iexp2))

mathexp :: Parser (Exp Int)
mathexp = chainl1 term plusminus

term :: Parser (Exp Int)
term = chainl1 factor muldiv

factor :: Parser (Exp Int)
factor = sign <|> (nat <|> (var <|> parens lis intexp))

plusminus :: Parser (Exp Int -> Exp Int -> Exp Int)
plusminus = do reservedOp lis "+"
               return Plus 
              <|> do reservedOp lis "-"
                     return Minus

muldiv :: Parser (Exp Int -> Exp Int -> Exp Int)
muldiv = do reservedOp lis "*"
            return Times 
           <|> do reservedOp lis "/"
                  return Div

sign :: Parser (Exp Int)
sign = do reservedOp lis "-"
          x <- factor
          return (UMinus x)

nat :: Parser (Exp Int)
nat = do x <- natural lis
         return (Const (fromIntegral x))

var :: Parser (Exp Int)
var = do x <- identifier lis
         return (Var x)

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolatom :: Parser (Exp Bool)
boolatom = values <|> (negboolatom <|> parens lis boolexp)

negboolatom :: Parser (Exp Bool)
negboolatom = do reservedOp lis "!"
                 x <- boolatom
                 return (Not x)

boolexp :: Parser (Exp Bool)
boolexp = chainl1 andexp orParse

andexp :: Parser (Exp Bool)
andexp = chainl1 negexp andParse

negexp :: Parser (Exp Bool)
negexp = do reservedOp lis "!"
            x <- negexp
            return (Not x)
           <|> comps

comps :: Parser (Exp Bool)
comps = values <|> (try(parens lis boolexp) <|> comp)

comp :: Parser (Exp Bool)
comp = do intexp1 <- intexp
          comp <- compOp
          intexp2 <- intexp
          return (comp intexp1 intexp2)
                                          
compOp :: Parser (Exp Int -> Exp Int -> Exp Bool)
compOp = do reservedOp lis "<"
            return Lt
           <|> do reservedOp lis ">"
                  return Gt
                 <|> do reservedOp lis "=="
                        return Eq
                       <|> do reservedOp lis "!="
                              return NEq

orParse :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orParse = do reservedOp lis "||"
             return Or

andParse :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andParse = do reservedOp lis "&&"
              return And

values :: Parser (Exp Bool)
values = do reserved lis "true"
            return BTrue
           <|> do reserved lis "false"
                  return BFalse

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 cmd semiOp

cmd :: Parser Comm
cmd = skipParse <|> (ifParse <|> (whileParse <|> varParse))

varParse :: Parser Comm
varParse = do var <- identifier lis
              reservedOp lis "="
              x <- intexp
              return (Let var x)


whileParse :: Parser Comm
whileParse = try (do reserved lis "while"
                     cond <- boolexp
                     com <- braces lis comm
                     return (While cond com))
 
ifParse :: Parser Comm
ifParse = try (do reserved lis "if"
                  cond <- boolexp
                  com1 <- braces lis comm
                  x <- (do reserved lis "else"
                           com2 <- braces lis comm
                           return (IfThenElse cond com1 com2)
                          <|> return (IfThen cond com1))
                  return x)

skipParse :: Parser Comm
skipParse = do reserved lis "skip"
               return Skip 

semiOp :: Parser (Comm -> Comm -> Comm)
semiOp = do reservedOp lis ";"
            return Seq

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
