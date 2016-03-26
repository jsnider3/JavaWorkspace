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
    deriving (Eq, Show)
    
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
        
exp_div ex d = case ex of
    N n -> N (quot n d)
    Var v -> Var v
    Neg x -> Neg (exp_div x d)
    Add a b -> Add (exp_div a d) (exp_div b d)
    Sub a b -> Sub (exp_div a d) (exp_div b d)
    Mul a b -> Mul (exp_div a d) b
    Div a (N n) -> exp_div (simplify a) (n * d)
    _ -> ex
    
exp_mul ex d = case ex of
    N n -> N (n * d)
    Var v -> Mul (N d) (Var v)
    Neg x -> Neg (exp_mul x d)
    Add a b -> Add (exp_mul a d) (exp_mul b d)
    Sub a b -> Sub (exp_mul a d) (exp_mul b d)
    Mul a b -> Mul (exp_mul a d) b
    Div a (N n) -> exp_mul (simplify a) (n * d)
    _ -> ex
    
simplify_once :: Expr -> Expr
simplify_once ex = case ex of
    N n -> N n
    Var v -> Var v
    Neg x -> Neg (simplify x)
    Add (N a) (N b) -> N (a + b)
    Add a b -> Add (simplify a) (simplify b)
    Sub (N a) (N b) -> N (a - b)
    Sub a b -> Sub (simplify a) (simplify b)
    Mul (N 1) b -> b
    Mul (N n) b -> exp_mul b n
    Mul a b -> simplify_mul (simplify a) (simplify b)
    Div a (N 1) -> simplify a
    Div a (N n) -> simplify $ exp_div (simplify a) n
    Div a b -> simplify $ Div (simplify a) (simplify b)
    Exp (Var v) (N n) -> ex
    Exp a (N 0) -> N 1
    Exp a (N 1) -> a
    Exp a (N n) -> simplify (Mul a (Exp a (N (n-1))))
    _ -> ex

simplify :: Expr -> Expr
simplify ex = let nex = simplify_once ex in case nex == ex of
    True -> nex
    False -> simplify nex

simplify_mul :: Expr -> Expr -> Expr
simplify_mul a b = poly_to_expr [poly_mul x y | x <- (summation_simplified a), y <- (summation_simplified b)]
    -- simplify a b, convert them to [Poly], multiply every pair, and convert back.

poly_add :: Poly -> Poly -> Poly
poly_add (Pol c1 n1) (Pol c2 n2) = Pol (c1 + c2) n1

poly_mul :: Poly -> Poly -> Poly
poly_mul (Pol c1 n1) (Pol c2 n2) = Pol (c1 * c2) (n1 + n2)

poly_pow :: Poly -> Integer
poly_pow pol = case pol of
  Pol c n -> n

poly_neg :: [Poly] -> [Poly]    
poly_neg [] = []
poly_neg [Pol c p] = [Pol (-c) p]
poly_neg (x:xs) = poly_neg [x] ++ poly_neg xs

poly_to_expr :: [Poly] -> Expr
poly_to_expr [Pol c n] = N 0 
poly_to_expr ((Pol c n):pols) = Add (N 0) (poly_to_expr pols) 

summation :: Expr -> [Poly]
summation ex = case ex of
    N n -> [Pol n 0]
    Var v -> [Pol 1 1]
    Neg x -> poly_neg $ summation x
    Add a b -> summation a ++ summation b
    Sub a b -> summation a ++ poly_neg (summation b)
    Mul (N n) (Var v) -> [Pol n 1]
    Exp (Var v) (N n) -> [Pol 1 n]
    _ -> error $ "Could not summarize: " ++ show ex

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
    print ([Pol 11 1, Pol (-2) 0] == summation_simplified (simplify (prettyParse "10x + 2x - (3x + 6)/3")))
    print (exp_mul (N 2) 18 == N 36)
    print (simplify (Div (Add (Mul (N 9) (Var "x")) (N 81)) (N 3)) == Add (Mul (N 3) (Var "x")) (N 27))
    print ([Pol 1 1, Pol 18 0] == summation_simplified (simplify ((Div (Add (Div (Add (Mul (N 9) (Var "x")) (N 81)) (N 3)) (N 27)) (N 3)))))
    print (N 3 == simplify (Add (Add (N 1) (N 1)) (N 1)))
    print (simplify $Sub (Div (Mul (Add (Mul (N 2) (Var "x")) (N 5)) (Add (Div (Mul (Var "x") (Add (Mul (N 9) (Var "x")) (N 81))) (N 3)) (N 27))) (Add (Add (N 1) (N 1)) (N 1))) (Mul (N 2) (Var "x")))
    --Div (Mul (Add (Mul (N 2) (Var "x")) (N 5)) (Add (Div (Mul (Var "x") (Add (Mul (N 9) (Var "x")) (N 81))) (N 3)) (N 27))) (Add (Add (N 1) (N 1)) (N 1))
    print "input"
    print (pretty (exprs !! 4))
    print "parsed"
    print (parseString $ pretty $ exprs !! 4)
    print "simplified"
    print (simplify $ parseString $ pretty $ exprs !! 4)
    print "polynomial"
    print (summation_simplified $ simplify $ parseString $ pretty $ exprs !! 4)
    mapM print (map (\x -> summation_simplified $ simplify $ prettyParse x) exprs)
