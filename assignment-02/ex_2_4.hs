-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

-- 2.4.1
--swap :: (a, b) -> (b, a)
swap :: (Int, Int) -> (Int, Int)
swap (a, b) = (b, a)

--sqr :: (a, b) -> (b, a)
sqr :: (Int, Int) -> (Int, Int)
sqr (a, b) = (a*a, b*b)

--add_mult :: (a, b) -> (b, a)
add_mult :: (Int, Int) -> (Int, Int)
add_mult (a, b) = (a+b, a*b)

-- 2.4.2
-- swap is still valid as the type can still correctly be inferred. This is not
-- the case with sqr and add_mult however due to their use of operators.

-- 2.4.3
-- (Int, (Char, Bool)) is a 2-tuple of Int and a 2-tuple of Char and Bool.
-- (Int, Char, Bool) is a 3-tuple of Int, Char and Bool.
convert_tuples :: (Int, (Char, Bool)) -> (Int, Char, Bool)
convert_tuples (i, (c, b)) = (i, c, b)
