module Chapter14 where

-- 1. 
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--   --mempty :: (a,b)
--   mempty = (mempty, mempty)
-- 
--   --mappend :: (a,b) -> (a,b) -> (a,b)
--   (x1, y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2) 

-- 2.
-- instance Monoid b => Monoid (a -> b) where
--  --mempty :: a -> b
--  mempty = \_ -> mempty 
-- 
--   --mappend :: (a -> b) -> (a -> b) -> (a -> b)
--   g `mappend` h = \x -> g x `mappend` h x 

-- 3.
instance Foldable Maybe where
  --foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Nothing = mempty
  foldMap g (Just x) = g x
  --foldr   :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ v _ Nothing = v
  foldr g v (Just x) = g x v 
  --foldl   :: (a -> b -> a) -> a -> Maybe b -> a
  foldl _ v Nothing = v
  foldl g v (Just x) = g v x 

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing  = pure Nothing
  traverse g (Just x) = fmap Just (g x)