{-# LANGUAGE Rank2Types #-}

module Goedel(test, test2, test3) where

import Prelude hiding (map, (++), foldl, sum)

newtype List a = List { fold :: forall b. (a -> b -> b) -> b -> b }

{-# INLINE nil #-}
nil = List $ \_ n -> n
{-# INLINE cons #-}
cons x xs = List $ \c n -> c x (fold xs c n)

{-# INLINE map #-}
map :: (a -> b) -> List a -> List b
map f xs = List $ \c n -> fold xs (\x xs -> f x `c` xs) n

{-# INLINE (++) #-}
(++) :: List a -> List a -> List a
xs ++ ys = List $ \c n -> fold xs c (fold ys c n)

{-# INLINE sum #-}
sum :: List Int -> Int
sum = foldl (+) 0

-- Look ma, no space leak!
-- And with foldl' there's still no space leak! Honest!
{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> List b -> a
foldl op e xs = fold xs (\x y z -> y (z `op` x)) id e

{-# INLINE toCh #-}
toCh xs = List $ \op e -> myFoldr op e xs

{-# INLINE myFoldr #-}
myFoldr op e = aux
  where aux [] = eta e
        aux (x:xs) = x `op` aux xs

{-# INLINE[0] eta #-}
eta x = x

{-# RULES
  "eta" forall x. eta x = \y -> x y
  #-}

{-# INLINE fromCh #-}
fromCh l = fold l (:) []

{-# NOINLINE test #-}
test :: (a -> b) -> (b -> c) -> [a] -> [c]
test f g xs = fromCh (map g (map f (toCh xs)))

{-# NOINLINE test2 #-}
test2 :: [Int] -> [Int] -> Int
test2 xs ys = sum (toCh xs ++ toCh ys)

{-# NOINLINE test3 #-}
test3 :: [Int] -> Int
test3 xs = sum (map square (toCh xs))
  where square x = x * x
