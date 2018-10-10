module Hardware
where

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

data Bit  =  O | I
  deriving (Eq, Ord, Show)

infixr 3 *&*
(*&*) :: Bit -> Bit -> Bit
O *&* _b  =  O
I *&* b   =  b

infixr 2 *|*
(*|*) :: Bit -> Bit -> Bit
O *|* b   =  b
I *|* _b  =  I

infixr 4 *+*
(*+*) :: Bit -> Bit -> Bit
O *+* O  =  O
O *+* I  =  I
I *+* O  =  I
I *+* I  =  O

-- 5.6.1
mapr :: ((a, state) -> (b, state)) -> (([a], state) -> ([b], state))
mapr f = \(as, s) -> foldr g ([],s) as
  where g a (bs, s) = let (b, sn) = f (a, s) in (b:bs, sn)

type Carry  =  Bit

-- 5.6.2
halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (a, b) = (a *+* b, a *&* b)

fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((a, b), c) = (x *+* c, y *|* ((a *|* b) *&* c))
  where (x, y) = halfAdder (a, b)

adder :: ([(Bit, Bit)], Carry) -> ([Bit], Carry)
adder (as, c) = mapr fullAdder (as, c)
