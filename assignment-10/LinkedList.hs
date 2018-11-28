module LinkedList
where
import Data.IORef
import Control.Monad

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

type ListRef elem  =  IORef (List elem)

data List elem  =  Nil | Cons elem (ListRef elem)

-- 10.4.1
nil  :: IO (ListRef elem)
nil = newIORef Nil

cons :: elem -> ListRef elem -> IO (ListRef elem)
cons e ref = newIORef $ Cons e ref

-- 10.4.2
fromList :: [elem] -> IO (ListRef elem)
fromList [] = nil
fromList (x:xs) = do { ref <- fromList xs; cons x ref }

toList   :: ListRef elem -> IO [elem]
toList ref = do { list <- readIORef ref; toList' list }

toList' :: List elem -> IO [elem]
toList' Nil = do return []
toList' (Cons e ref) = do { elems <- toList ref; return (e : elems) }

-- 10.4.3
foreach :: ListRef a -> (a -> IO b) -> IO (ListRef b)
foreach ref f = do { list <- readIORef ref; foreach' list f }

foreach' :: List a -> (a -> IO b) -> IO (ListRef b)
foreach' Nil f = nil
foreach' (Cons a ref) f = do { b <- f a; bs <- foreach ref f; cons b bs }

-- action must return the same type in order to overwrite old value, and
-- the return value is no longer needed
foreachO :: ListRef a -> (a -> IO a) -> IO ()
foreachO ref f = do
    list <- readIORef ref
    case list of
      Nil        -> return ()
      (Cons a r) -> do b <- f a; writeIORef ref (Cons b r); foreachO r f
