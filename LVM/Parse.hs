module LVM.Parse where
import Text.Parsec hiding (parse, space) -- Defining our own of these
import qualified Text.Parsec (parse) -- We still want to be able to use this
import Text.Parsec.String (Parser)
import System.Console.Readline
import Data.Char (isDigit)
import Control.Monad

data Expr = Var String | App Expr Expr | Nat Int | Lam String Expr | Boolean Bool | IR Expr -- IR is irreducible, for Reduce
    deriving Eq
instance Show Expr where
    show (Var x) = '\'':x ++ "' : Symbol"
    show (Nat x) = show x ++ " : Nat"
    show (App f x) = '(':(show f) ++ ") (" ++ show x ++ ")"
    show (Lam x e) = "\\" ++ x ++ " -> " ++ show e
    show (Boolean x) = show x ++ " : Bool"
    show (IR x) = show x

space :: Parser ()
space = void $ many $ oneOf " \t"

spaceN :: Parser () -- Because sometimes you want newlines, and sometimes you don't.
spaceN = void $ many $ oneOf " \t\n"

nat :: Parser Expr -- Parses natural numbers
nat = do
    n <- many1 digit
    return (Nat $ read n)

ident :: Parser Expr -- Parses identifiers
ident = do
    i0 <- noneOf "0123456789 \t\n()[]{}:"
    i1 <- many $ noneOf " \t\n()[]{}:"
    return $ Var (i0:i1)

paren :: Parser Expr
paren = do
    space
    char '('
    space
    e <- expr -- Implicit space
    char ')'
    return e

true :: Parser Expr
true = do
    string "True"
    notFollowedBy (noneOf " \t\n(){}[]")
    return $ Boolean True

false :: Parser Expr
false = do
    string "False"
    notFollowedBy (noneOf " \t\n(){}[]")
    return $ Boolean False

partApp :: Expr -> Parser Expr
partApp f = do
    space
    x <- true <|> false <|> lam <|> nat <|> ident <|> paren
    z <- option (App f x) $ try (partApp (App f x))
    return z

app :: Parser Expr
app = do
    f <- true <|> false <|> lam <|> nat <|> ident <|> paren
    space
    x <- partApp f
    return $ x

lam :: Parser Expr
lam = do
    char '\\'
    space
    (Var free) <- ident
    space
    char '-'
    char '>'
    space
    e <- expr
    return $ Lam free e

expr :: Parser Expr
expr = do
    e <- true <|> false <|> lam <|> try app <|> nat <|> ident <|> paren
    space
    return e

assign :: Parser (String, Expr)
assign = do
    Var name <- ident
    space
    char '='
    space
    e <- expr
    return (name, e)

empty :: Parser (String, Expr)
empty = do
    e <- expr
    return ("", e)

lvmParser :: Parser (String, Expr)
lvmParser = do
    spaceN
    stuff <- try assign <|> empty
    return stuff
    
parse :: String -> (String, Expr)
parse x = case Text.Parsec.parse (lvmParser <* eof) "" x of
    Right result -> result
    Left error -> ("Error " ++ show error, Nat 1)