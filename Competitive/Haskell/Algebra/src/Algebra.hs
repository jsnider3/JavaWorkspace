import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Regex
import qualified Text.ParserCombinators.Parsec.Token as Token

tern :: Bool -> a -> a -> a
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
    
data Poly = Pol Integer Integer deriving Show

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
    _ -> Mul (N d) ex
    
simplify_once :: Expr -> Expr
simplify_once ex = case ex of
    N n -> N n
    Var v -> Var v
    Neg (N n) -> N (-n)
    Neg x -> Neg (simplify x)
    Add (N 0) b -> simplify b
    Add a (N 0) -> simplify a
    Add (N a) (N b) -> N (a + b)
    Add a b -> Add (simplify a) (simplify b)
    Sub (N 0) b -> Neg (simplify b)
    Sub a (N 0) -> simplify a
    Sub (N a) (N b) -> N (a - b)
    Sub a b -> Sub (simplify a) (simplify b)
    Mul (N 1) b -> b
    Mul (N n) b -> exp_mul b n
    Mul a b -> simplify_mul (simplify a) (simplify b)
    Div a (N 1) -> simplify a
    Div a (N n) -> simplify $ exp_div (simplify a) n
    Div a b -> simplify $ Div (simplify a) (simplify b)
    Exp (Var v) (N n) -> ex
    Exp (N a) (N b) -> N (a ^ b)
    Exp a (N 0) -> N 1
    Exp a (N 1) -> a
    Exp a (N n) -> simplify (Mul a (Exp a (N (n-1))))
    Exp a b -> Exp (simplify a) (simplify b)
    _ -> error $ "no pattern for " ++ show ex

simplify :: Expr -> Expr
simplify ex = let nex = simplify_once ex in case nex == ex of
    True -> nex
    False -> simplify nex

simplify_mul :: Expr -> Expr -> Expr
simplify_mul a b = poly_to_expr [poly_mul x y | x <- (summation_simplified a), y <- (summation_simplified b)]

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
poly_to_expr [Pol c n] = Mul (N c) (Exp (Var "x") (N n))
poly_to_expr ((Pol c n):pols) = Add (Mul (N c) (Exp (Var "x") (N n))) (poly_to_expr pols) 

summation :: Expr -> [Poly]
summation ex = case ex of
    N n -> [Pol n 0]
    Var v -> [Pol 1 1]
    Neg x -> poly_neg $ summation x
    Add a b -> summation a ++ summation b
    Sub a b -> summation a ++ poly_neg (summation b)
    Mul (N n) (Var v) -> [Pol n 1]
    Mul (N n) (Exp (Var v) (N p)) -> [Pol n p]
    Exp (Var v) (N n) -> [Pol 1 n]
    _ -> error $ "Could not summarize: " ++ show ex

summation_add :: [Poly] -> [Poly]
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
spaceVar str = case ((isNumber (head str) && head (tail str) `elem` ['x', '(']) ||
                     (head str == 'x' && head(tail str) == '(')) of
    True -> head str : ('*' : spaceVar (tail str))
    False -> head str : spaceVar (tail str)

pretty str = spaceVar (subRegex (mkRegex " ") (subRegex (mkRegex "\\)\\(") (spaceVar str) ")*(") "")

prettyParse :: String -> Expr
prettyParse str = parseString (pretty str)

{-
  poly_str :: [Poly] -> String
    Convert a polynomial to pretty text.
-}
poly_str :: [Poly] -> String
poly_str polys = intercalate " " ((poly_str_hd (head polys)) : map poly_str_tl (tail polys))

poly_str_hd (Pol c p) = tern (signum c == 1) "" "-" ++
                        tern (abs c /= 1) (show (abs c)) "" ++
                        tern (p /= 0) "x" "" ++
                        tern (p /= 0 && abs p /= 1) ("^" ++ show p) ""

poly_str_tl (Pol c p) = tern (signum c == 1) "+ " "- " ++
                        tern (abs c /= 1) (show (abs c)) "" ++
                        tern (p /= 0) "x" "" ++
                        tern (p /= 0 && abs p /= 1) ("^" ++ show p) ""

test = do
    ln <- getLine
    exprs <- replicateM (read ln) (getLine)
    print ([Pol 11 1, Pol (-2) 0] == summation_simplified (simplify (prettyParse (exprs !! 0))))
    print ([Pol 36 1, Pol 31 0] == summation_simplified (simplify (prettyParse (exprs !! 1))))
    print ([Pol (-1) 1, Pol 18 0] == summation_simplified (simplify (prettyParse (exprs !! 2))))
    print ([Pol 12 2, Pol 47 1, Pol 20 0] == summation_simplified (simplify (prettyParse (exprs !! 3))))
    print ([Pol 2 3, Pol 23 2, Pol 61 1, Pol 45 0] == summation_simplified (simplify (prettyParse (exprs !! 4))))
    print ([Pol 2 5, Pol 5 4, Pol 18 2, Pol 61 1, Pol 45 0] == summation_simplified (simplify (prettyParse (exprs !! 5))))
    print ([Pol 7 1, Pol (-1) 0] == summation_simplified (simplify (prettyParse "5x + 2(x-4)")))

main = do
    ln <- getLine
    exprs <- replicateM (read ln) (getLine)
    mapM putStrLn (map (\x -> poly_str (summation_simplified (simplify (prettyParse x)))) exprs)
