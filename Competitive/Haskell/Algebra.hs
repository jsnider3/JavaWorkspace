-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Regex
import qualified Text.ParserCombinators.Parsec.Token as Token


tern b a x = case b of
  True -> a
  False -> x

data Expr = N Integer
    | Var String
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Exp Expr Expr
    | Pow Expr
    deriving Show
    
data Poly = Pol Integer Integer

instance Show Poly where
     show (Pol c1 n1) =  tern (abs c1 /= 1) (show c1) "" ++
        tern (n1 == 0) ("") ("x" ++
          tern (abs n1 /= 1) ("^" ++ show n1) (""))

instance Eq Poly where
     Pol c1 n1 == Pol c2 n2 = c1 == c2 && n1 == n2

instance Ord Poly where
    compare (Pol _ n1) (Pol _ n2) = compare n1 n2

arith = emptyDef {Token.identStart = letter, Token.reservedOpNames = ["-", "*", "+", "/", "^"]}
lexer = Token.makeTokenParser arith
xvar = Token.identifier lexer
op = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer

terms = parens expression
    <|> liftM Var xvar
    <|> liftM N integer
    
ops = [[Prefix (op "-" >> return (Neg))],
       [Infix (op "^" >> return (Exp)) AssocLeft],
       [Infix (op "*" >> return (Mul)) AssocLeft,
       Infix (op "/" >> return (Div)) AssocLeft],
       [Infix (op "+" >> return (Add)) AssocLeft,
       Infix (op "-" >> return (Sub)) AssocLeft]]

expression :: Parser Expr
expression = buildExpressionParser ops terms
 
parseString :: String -> Expr
parseString str =
    case parse expression "" str of
        Left e  -> error $ show e
        Right r -> r
        
poly_div ex d = case ex of
    N n -> N (quot n d)
    Var v -> Var v
    Neg x -> Neg (poly_div x d)
    Add a b -> Add (poly_div a d) (poly_div b d)
    Sub a b -> Sub (poly_div a d) (poly_div b d)
    Mul a b -> Mul (poly_div a d) b
    Div a (N n) -> poly_div (simplify a) (n * d)
    _ -> ex
    
poly_mul ex d = case ex of
    N n -> N (n * d)
    Var v -> Mul (N d) (Var v)
    Neg x -> Neg (poly_mul x d)
    Add a b -> Add (poly_mul a d) (poly_mul b d)
    Sub a b -> Sub (poly_mul a d) (poly_mul b d)
    Mul a b -> Mul (poly_mul a d) b
    Div a (N n) -> poly_mul (simplify a) (n * d)
    _ -> ex
    
simplify :: Expr -> Expr
simplify ex = case ex of
    N n -> N n
    Var v -> Var v
    Neg x -> Neg (simplify x)
    Add a b -> Add (simplify a) (simplify b)
    Sub a b -> Sub (simplify a) (simplify b)
    Mul (N 1) b -> b
    Mul (N n) b -> poly_mul b n
    Mul a b -> Mul (simplify a) (simplify b)
    Div a (N n) -> simplify $ poly_div (simplify a) n
    Exp (Var v) (N n) -> ex
    Exp a (N 0) -> N 1
    Exp a (N 1) -> a
    Exp a (N n) -> simplify (Mul a (Exp a (N (n-1))))
    _ -> ex

poly_add :: Poly -> Poly -> Poly
poly_add (Pol c1 n1) (Pol c2 n2) = Pol (c1 + c2) n1

poly_pow :: Poly -> Integer
poly_pow pol = case pol of
  Pol c n -> n

poly_neg :: [Poly] -> [Poly]    
poly_neg [] = []
poly_neg [Pol c p] = [Pol (-c) p]
poly_neg (x:xs) = poly_neg [x] ++ poly_neg xs

summation :: Expr -> [Poly]
summation ex = case ex of
    Neg x -> poly_neg $ summation x
    N n -> [Pol n 0]
    Var v -> [Pol 1 1]
    Neg x -> poly_neg $ summation x
    Add a b -> summation a ++ summation b
    Sub a b -> summation a ++ poly_neg (summation b)
    Mul (N n) (Var v) -> [Pol n 1]
    Exp (Var v) (N n) -> [Pol 1 n]
    _ -> error $ show ex

summation_add [] = []
summation_add [p] = case p of
    Pol 0 _ -> []
    _ -> [p]

summation_add (p1:(p2:x)) = case poly_pow p1 == poly_pow p2 of
    True -> summation_add ((poly_add p1 p2):x)
    False -> case p1 of
        Pol 0 _ -> summation_add (p2:x)
        _ -> p1 : summation_add (p2:x)

summation_simplify pols = summation_add $ sort pols
summation_simplified ex = reverse $ summation_add $ sort $ summation ex

spaceVar [] = []
spaceVar [x] = [x]
spaceVar str = case isNumber (head str) && head (tail str) == 'x' of
    True -> head str : ('*' : spaceVar (tail str))
    False -> head str : spaceVar (tail str)

pretty str = subRegex (mkRegex "\\)\\(") (spaceVar str) ")*("

prettyParse :: String -> Expr
prettyParse str = parseString (pretty str)

main = do
    ln <- getLine
    exprs <- replicateM (read ln) (getLine)
    --mapM print (map pretty exprs)
    --mapM print (map (\x -> parseString $ pretty x) exprs)
    --mapM print (map (\x -> simplify $ parseString $ pretty x) exprs)
    --print (simplify (Var "x"))
    --print (simplify (N 10))
    --print (simplify $ (Mul (N 10) (Var "x")))
    --print (simplify $ Add (Mul (N 10) (Var "x"))(Mul (N 2) (Var "x")))
    --print $ summation_simplified $ simplify $ prettyParse "10x + 2x - (3x + 6)/3"
    print ([Pol 11 1, Pol (-2) 0] == summation_simplified (simplify (prettyParse "10x + 2x - (3x + 6)/3")))
    --print (simplify $ (Div (Add (Mul (N 3) (Var "x")) (N 6)) (N 3)))
    print (pretty (exprs !! 1))
    print (simplify $ (Add (Mul (N 2) (Var "x")) (N 2)))
    print (poly_mul (Mul (N 2) (Var "x")) 18)
    print (poly_mul (N 2) 18)
    print (poly_mul (Add (Mul (N 2) (Var "x")) (N 2)) 18)
    print (simplify $ Mul (N 18) (Add (Mul (N 2) (Var "x")) (N 2)))
    print (parseString $ pretty $ exprs !! 1)
    print (simplify $ parseString $ pretty $ exprs !! 1)
    mapM print (map (\x -> summation_simplified $ simplify $ prettyParse x) exprs)
