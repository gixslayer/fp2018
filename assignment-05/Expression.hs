module Expression
where
import Prelude hiding (fail)
import Parser

infixl 6 :+:
infixl 7 :*:

data Expr
  =  Lit Integer    -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  deriving (Show)

--An expression parser using >>=.

expr, term, factor :: Parser Expr
expr    =   term
        .|  (term >>= \ i -> symbol '+' >> expr >>= \ j -> return (i :+: j))
term    =   factor
        .|  (factor >>= \ i -> symbol '*' >> term >>= \ j -> return (i :*: j))
factor  =   (many1 digit >>= \ ds -> return (Lit (read ds)))
        .|  (symbol '(' >> expr >>= \ i -> symbol ')' >> return i)

--Using do-notation.

expr', term', factor' :: Parser Expr
expr'    =   do term'
         .|  do i <- term' ; symbol '+' ; j <- expr' ; return (i :+: j)
term'    =   do factor'
         .|  do i <- factor' ; symbol '*' ; j <- term' ; return (i :*: j)
factor'  =   do ds <- many1 digit ; return (Lit (read ds))
         .|  do symbol '(' ; i <- expr' ; symbol ')' ; return i


-- 5.3.1
-- Actually infinity recurses, but not sure what this assignment actually wants
-- as the alternative using expr/term/factor is already given...
expr1 :: Parser Expr
expr1 =  do ds <- many1 digit ; return (Lit (read ds))
      .| do i <- expr1 ; symbol '+' ; j <- expr1 ; return (i :+: j)
      .| do i <- expr1 ; symbol '*' ; j <- expr1 ; return (i :*: j)
      .| do symbol '(' ; i <- expr1 ; symbol ')' ; return i

--parse expr "4*71+1"
--parse expr "4 * 71 + 1"
