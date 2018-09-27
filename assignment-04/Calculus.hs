module Calculus
where

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

-- 4.6.1
data Primitive
  =  Sin  -- trigonometric: sine
  |  Cos  -- trigonometric: cos
  |  Exp  -- exponential
  deriving (Show)

infixl 6 :+:

data Function
  =  Const Rational         -- constant function
  |  Id                     -- identity
  |  Prim Primitive         -- primitive function
  |  Function :+: Function  -- addition of functions
  |  Function :*: Function  -- multiplication of functions
  |  Function :.: Function  -- composition of functions
  |  Function :^: Int       -- powers
  deriving (Show)

--infixl 7 :*:
--infixr 9 :.:

-- 4.6.2
apply    :: Function -> (Double -> Double)
apply (Const r) = \x -> fromRational r
apply Id = id
apply (l :+: r) = \x -> apply l x + apply r x
apply (l :*: r) = \x -> apply l x * apply r x
apply (l :.: r) = \x -> apply l $ apply r x
apply (l :^: r) = \x -> apply l x ^ r
apply (Prim Sin) = sin
apply (Prim Cos) = cos
apply (Prim Exp) = exp

-- 4.6.3
derive   :: Function -> Function
derive (Const _) = Const 0
derive Id = Const 1
derive (l :+: r) = derive l :+: derive r
derive ((Const l) :*: r) = Const l :*: derive r
derive (l :*: r) = (derive l :*: r) :+: (l :*: derive r)
derive (l :.: r) = (derive l :.: r) :*: derive r
derive (l :^: r) = (Const (toRational r)) :*: (l :^: (r-1))
derive (Prim Sin) = Prim Cos
derive (Prim Cos) = (Const (-1)) :*: (Prim Sin)
derive (Prim Exp) = Prim Exp

-- 4.6.4
-- Obviously not exhaustive, but enough to simplify the examples
-- simplify $ derive (Const 1 :+: Const 2 :*: Id) -> Const (2 % 1)
-- simplify $ derive (Id :.: Id :.: Id)           -> Const (1 % 1)
simplify :: Function -> Function
simplify (Id :.: Id) = Id
simplify (f :.: Id) = simplify f
simplify (Const l :+: Const r) = Const (l + r)
simplify (Const 0 :+: f) = simplify f
simplify (f :+: Const 0) = simplify f
simplify (Const l :*: Const r) = Const (l * r)
simplify (Const 1 :*: f) = simplify f
simplify (f :*: Const 1) = simplify f
simplify (Const 0 :*: f) = Const 0
simplify (f :*: Const 0) = Const 0
simplify f = f
