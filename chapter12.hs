module Chapter12 where

-- 1.
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)     -> Leaf (g x)
  fmap g (Node l x r) -> Node (fmap g l) (g x) (fmap g r)

-- 2.
instance Functor ((->) a) where
  -- fmap (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

-- 3.
instance Applicative ((->) p) where
  -- pure :: a -> f a
  -- pure :: a -> (->) p a
  -- pure :: a -> (p -> a)
  -- pure :: a -> p -> a
  pure = const
  
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*>) :: ((->) p (a -> b)) -> ((->) p a) -> ((->) p b) 
  -- (<*>) :: (p -> (a -> b)) -> (p -> a) -> (p -> b) 
  -- (<*>) :: (p -> a -> b) -> (p -> a) -> (p -> b)
  g (<*>) h = \x -> g x (h x)
