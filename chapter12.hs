module Chapter12 where

-- 1.
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

--instance Functor Tree where
--  -- fmap (a -> b) -> Tree a -> Tree b
--  fmap g (Leaf x)     = Leaf (g x)
--  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- 2.
--instance Functor ((->) a) where
--  -- fmap (b -> c) -> (a -> b) -> (a -> c)
--  fmap = (.)

-- 3.
--instance Applicative ((->) p) where
--  -- pure :: a -> f a
--  -- pure :: a -> (->) p a
--  -- pure :: a -> (p -> a)
--  -- pure :: a -> p -> a
--  pure = const
  
--  -- (<*>) :: f (a -> b) -> f a -> f b
--  -- (<*>) :: ((->) p (a -> b)) -> ((->) p a) -> ((->) p b) 
--  -- (<*>) :: (p -> (a -> b)) -> (p -> a) -> (p -> b) 
--  -- (<*>) :: (p -> a -> b) -> (p -> a) -> (p -> b)
--  g <*> h = \x -> g x (h x)

-- 4.
newtype ZipList a = Z [a]
                    deriving Show

instance Functor ZipList where
  -- fmap (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) =  Z (fmap g xs)

instance Applicative ZipList where
  -- pure :: a -> f a
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- <*> :: f (a -> b) -> f a -> f b
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z hs) = Z [g h | (g, h) <- zip gs hs] 

-- 6.
-- instance Monad ((-> p)) where
--   -- (>>=) :: f a -> (a -> f b) -> f b
--   -- (>>=) :: (->) p a -> (a -> (->) p b) -> (->) p b
--   -- (>>=) :: (p -> a) -> (a -> (p -> b)) -> (p -> b)
--   g >>= h = \x -> h (g x) x

-- 7.
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var x) = Var (g x)
  fmap _ (Val x) = Val x
  fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
  -- pure :: a -> f a
  -- pure :: a -> Expr a
  pure x = Var x

  -- <*> :: f (a -> b) -> f a -> f b
  -- <*> :: Expr (a -> b) -> Expr a -> Expr b
  Var g <*> ex = fmap g ex

instance Monad Expr where
  -- (>>=) :: f a -> (a -> f b) -> f b
  Var x >>= g = g x
  Val x >>= _ = Val x

-- 8.
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do x <- st
                 S (\s -> ((g x), s))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do f <- stf
                   x <- stx
                   S (\s -> ((f x), s))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
    let (x, s') = app st s in app (f x) s')