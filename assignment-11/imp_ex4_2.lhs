2d implies 3:
< pure f <*> u <*> pure x = {2d; wrong associativity but we were told this does not matter, idk why}
< pure f <*> pure (\g -> g x) <*> u = {2c}
< pure (f (\g -> g x)) <*> u = {rewrite: f (\g -> g x) h = f h x = flip f x h}
< pure (flip f x) <*> u = {2c}
< pure (flip f) <*> pure x <*> u

3 implies 2d:
< u <*> pure x = {2a}
< pure id <*> u <*> pure x = {3}
< pure (flip id) <*> pure x <*> u = {2c}
< pure (flip id x) <*> u = {rewrite: flip id x h = id h x = id (\g -> g x) h}
< pure (\g -> g x) <*> u

Due to the flip context have an instance of id with type
< id :: (a -> b) -> (a -> b) or (a -> b) -> a -> b
So we have:
< flip id :: a -> (a -> b) -> b
