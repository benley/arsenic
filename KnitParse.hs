module KnitParse where

import Control.Monad (void, forM_)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data Row = WrongSide Int [Command]
         | RightSide Int [Command]
         deriving Show

data Command =
    K
    | K2Tog
    | K3Tog
    | P
    | P2Tog
    | Slppsso
    | Slkpsso
    | Ssk
    | Yo
    deriving Show

sc :: Parser ()
sc = L.space (void (char ' ')) lineComment blockComment where
  lineComment = L.skipLineComment "//" <|> L.skipLineComment "#"
  blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

knit :: Parser [Command]
knit = try knit3 <|> try knit2 <|> do
    char 'k'
    n <- read <$> lexeme (some digitChar)
    return (replicate n K)

knit2 :: Parser [Command]
knit2 = lexeme (string "k2") >> lexeme (string "tog") >> return [K2Tog]

knit3 :: Parser [Command]
knit3 = lexeme (string "k3") >> lexeme (string "tog") >> return [K3Tog]

purl :: Parser [Command]
purl = try purl2 <|> do
    char 'p'
    n <- read <$> lexeme (some digitChar)
    return (replicate n P)

ktoken x = lexeme (string x)

purl2 :: Parser [Command]
purl2 = ktoken "p2" >> ktoken "tog" >> return [P2Tog]

sl1 :: Parser [Command]
sl1 = do
    ktoken "sl"
    choice [ ktoken "1-k2" >> ktoken "tog-psso"  >> return [Slkpsso]
           , ktoken "1" >> ktoken "wyif-p1-psso" >> return [Slppsso]
           ]

parenExpr :: Parser [Command]
parenExpr = inParens (concat <$> parseExpr `sepBy` ktoken ",")

twiceExpr :: Parser [Command]
twiceExpr = (concat . replicate 2) <$> (parenExpr <* ktoken "twice")

parseExpr :: Parser [Command]
parseExpr = choice [starsExpr
                   , twiceExpr
                   , ktoken "yo"  >> return [Yo]
                   , ktoken "ssk" >> return [Ssk]
                   , sl1
                   , purl
                   , knit]

parseMany :: Parser [Command]
parseMany = concat <$> (parseExpr `sepBy` ktoken ",")

inParens :: Parser a -> Parser a
inParens p = ktoken "(" *> p <* ktoken ")"

starsExpr :: Parser [Command]
starsExpr = do
    ktoken "*"
    cmds <- parseMany
    ktoken "*"
    repN <- read <$> lexeme (some digitChar)
    ktoken "x"
    return (concat $ replicate repN cmds)

parseLine :: Parser Row
parseLine = do
    ktoken "Row"
    rowN <- read <$> lexeme (some digitChar)
    side <- inParens (ktoken "rs" <|> ktoken "ws")
    ktoken ":"
    cmds <- parseMany
    return $ case side of
      "rs" -> RightSide rowN cmds
      "ws" -> WrongSide rowN cmds

parseFile :: Parser [Row]
parseFile = (parseLine `sepEndBy` char '\n') <* eof

justParse :: FilePath -> String -> [Row]
justParse filename input =
  case runParser parseFile filename input of
    Right v -> v
    Left err -> error (parseErrorPretty err)
