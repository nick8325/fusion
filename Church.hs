-- Question: why doesn't it work if you use Church cons/nil directly
-- instead of build? They're both non-recursive.

{-# LANGUAGE Rank2Types #-}

module Church(test, test2, test3, test4, test5, test6) where

import Prelude hiding (map, (++), foldl, sum, tail, zipWith, reverse)

newtype List a = List { fold :: forall b. (a -> b -> b) -> b -> b }

{-# INLINE nil #-}
nil = List $ \_ n -> n
{-# INLINE cons #-}
cons x xs = List $ \c n -> c x (fold xs c n)

{-# INLINE map #-}
map :: (a -> b) -> List a -> List b
map f xs = List $ \c n -> fold xs (\x xs -> f x `c` xs) n

{-# INLINE tail #-}
tail xs = List $ \c n -> snd (fold xs (\x (ys, _) -> (x `c` ys, ys)) (n, error "oops"))

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
  "eta" forall x. eta x = \y -> eta (x y)
  #-}

{-# INLINE fromCh #-}
fromCh l = fold l (:) []

newtype CPS a = CPS { uncons :: forall b. (a -> CPS a -> b) -> b -> b }

{-# INLINE nilCPS #-}
nilCPS = CPS $ \_ n -> n
{-# INLINE consCPS #-}
consCPS x xs = CPS $ \c n -> c x xs

{-# INLINE zipWith #-}
zipWith f xs ys = List $ \c n ->
  fold xs (\x xs ys -> uncons ys (\y ys -> f x y `c` xs ys) n) (const n) (cps ys)

{-# INLINE zip #-}
zip = zipWith (,)

cps :: List a -> CPS a
cps xs = fold xs consCPS nilCPS

{-# INLINE reverse #-}
reverse xs = List $ \c n -> foldl (flip c) n xs

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

{-# NOINLINE test4 #-}
test4 :: (a -> b) -> (b -> c) -> [a] -> [c]
test4 f g xs = fromCh (map g (tail (map f (toCh xs))))

{-# NOINLINE test5 #-}
test5 :: [Int] -> [Int] -> Int
test5 xs ys = sum (zipWith (*) (toCh xs) (toCh ys))

{-# NOINLINE test6 #-}
test6 :: [a] -> [a] -> [a]
test6 xs ys = fromCh (toCh xs ++ (reverse (toCh ys) ++ toCh xs))