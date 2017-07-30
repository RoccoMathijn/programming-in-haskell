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
