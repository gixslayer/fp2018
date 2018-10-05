module Lambda
where
import Prelude
import Parser

-- 5.4.1

{-
lambda          ::= {term ' '} term
                  | '\' var '->' lambda
term            ::= var
                  | '(' lambda ')'
var             ::= idStart {idMid}
idStart         ::= lower | upper | '_'
idMid           ::= idStart | digit | "'"

Does not support λ/.-syntax
Does not accept whitespace in other locations than function application,
  in which case whitespace (one space) is required
-}

infixl 9 :@

data Lambda var
  =  Var var                   -- variable
  |  Fun var (Lambda var)      -- abstraction/λ-expression
  |  Lambda var :@ Lambda var  -- application
  deriving (Show)

lambda, term :: Parser (Lambda String)
lambda  = (many (term >>= \t -> symbol ' ' >> return t) >>= \ts -> term >>= \t -> return $ foldr (:@) t ts)
       .| (symbol '\\' >> var >>= \v -> symbol '-' >> symbol '>' >> lambda >>= \l -> return $ Fun v l)
term    = (var >>= \v -> return $ Var v)
       .| (symbol '(' >> lambda >>= \l -> symbol ')' >> return l)

var :: Parser String
var     = idStart >>= \s -> many idMid >>= \i -> return $ s : i

idStart, idMid :: Parser Char
idStart = lower .| upper .| symbol '_'
idMid   = idStart .| digit .| symbol '\''


-- 5.4.2

-- parse lambda "x (\\a->b (\\z->(y))) (Henk42_')"
-- Var "x" :@ (Fun "a" (Var "b" :@ Fun "z" (Var "y")) :@ Var "Henk42_'")

-- a\x->x is not accepted. which is correct
