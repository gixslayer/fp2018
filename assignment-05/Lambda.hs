module Lambda
where
import Prelude hiding (fail)
import Parser

infixl 9 :@

data Lambda var
  =  Var var                   -- variable
  |  Fun var (Lambda var)      -- abstraction/lambda-expression
  |  Lambda var :@ Lambda var  -- application
  deriving (Show)
