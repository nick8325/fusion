{-# LANGUAGE Rank2Types #-}

module List(test) where

import Prelude(undefined, ($))

newtype List a = List { fold :: forall b. (a -> b -> b) -> b -> b }

{-# INLINE nil #-}
nil = List $ \_ n -> n
{-# INLINE cons #-}
cons x xs = List $ \c n -> c x (fold xs c n)

foldList :: List a -> (a -> List b -> List b) -> List b -> List b
foldList xs op e = List $ \c n -> fold xs (\y ys -> fold (op y ys) c n) (fold e c n)

{-# INLINE map #-}
map :: (a -> b) -> List a -> List b
-- map f xs = List $ \c n -> fold xs (\x xs -> f x `c` xs) n
map f xs = foldList xs (\x xs -> f x `cons` xs) nil

{-# NOINLINE test #-}
test :: (a -> b) -> (b -> c) -> List a -> List c
test f g xs = map g (map f xs)