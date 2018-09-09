-- 2.1.1
-- There are 4 functions in total, namely
-- False -> False
-- False -> True
-- True  -> False
-- True  -> True
b2b_ff False = False
b2b_ft False = True
b2b_tf True = False
b2b_tt True = True

-- 2.1.2
-- There are 8 function in total, as there are 2^2=4 possible inputs, 
-- and for each input there are 2 possible outputs, thus 4*2=8.
bb2b_1 a b = a && b

bb2b_2 False b = b
bb2b_2 True  b = not b

bb2b_3 a b = if a then b else a 

bb2b_4 a b
    | a && b = True
    | not a && not b = True
    | otherwise = False

-- 2.1.3
-- There are 4 functions of type Bool -> Bool, hence a function taking a Bool 
-- and producing a function of type Bool -> Bool can do so in 2*4=8 ways.