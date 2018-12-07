module Evaluator
where
import Data.Maybe

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

infixl 6 :+:
infixl 7 :*:
infixr 1 :?:

data Expr
  =  Lit Integer    -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  |  Div Expr Expr  -- integer division
  |  Expr :?: Expr  -- non-deterministic choice
  |  Var String     -- a variable

evalA :: (Applicative f) => Expr -> f Integer
evalA (Lit i)      =  pure i
evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2

toss  ::  Expr
toss  =  Lit 0 :?: Lit 1

-- 11.1
evalN :: Expr -> [Integer]
evalN (Lit i)     = pure i
evalN (e1 :+: e2) = pure (+) <*> evalN e1 <*> evalN e2
evalN (e1 :*: e2) = pure (*) <*> evalN e1 <*> evalN e2
evalN (Div e1 e2) = pure div <*> evalN e1 <*> evalN e2
evalN (e1 :?: e2) = evalN e1 ++ evalN e2
--evalN (e1 :?: e2) = concat $ pure (\x y -> x : y : []) <*> evalN e1 <*> evalN e2

test_1 :: IO ()
test_1 = do
  putStrLn $ show $ evalN toss
  putStrLn $ show $ evalN (toss :+: Lit 2 :*: toss)
  putStrLn $ show $ evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))
  return ()

-- 11.2
evalR :: Expr -> [(String, Integer)] -> Integer
evalR (Lit i)     = pure i
evalR (e1 :+: e2) = pure (+) <*> evalR e1 <*> evalR e2
evalR (e1 :*: e2) = pure (*) <*> evalR e1 <*> evalR e2
evalR (Div e1 e2) = pure div <*> evalR e1 <*> evalR e2
evalR (Var var)   = pure (fromMaybe 0) <*> lookup var

test_2 :: IO ()
test_2 = do
  putStrLn $ show $ evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
  putStrLn $ show $ evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
  putStrLn $ show $ evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]
  return ()
