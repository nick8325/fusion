{-# LANGUAGE Rank2Types #-}

module Church(test, test3, test3a, test4) where

import Prelude hiding (map, (++), foldl, sum, tail, zipWith, reverse, succ, pred, foldr)

newtype Nat = Nat { foldn :: forall a. (Nat -> a -> a) -> a -> a }

{-# INLINE zero #-}
zero = Nat $ \_ z -> z
{-# INLINE succ #-}
succ n = Nat $ \s z -> s n (foldn n s z)
{-# INLINE pred #-}
pred n = Nat $ \s z -> foldn n (\p _ -> foldn p s z) z

newtype List a = List { foldDrop :: forall b. Nat -> (a -> b -> b) -> b -> b }

{-# INLINE nil #-}
nil = List $ \_ _ n -> n
{-# INLINE cons #-}
cons x xs = List $ \k c n ->
  foldn k
    (\m _ -> foldDrop xs m c n)
    (c x (foldDrop xs zero c n))

{-# INLINE map #-}
map :: (a -> b) -> List a -> List b
map f xs = List $ \k c n -> foldDrop xs k (\x xs -> f x `c` xs) n

{-# INLINE tail #-}
tail xs = List $ \k c n -> foldDrop xs (succ k) c n

-- {-# INLINE (++) #-}
-- (++) :: List a -> List a -> List a
-- xs ++ ys = List $ \c n -> fold xs c (fold ys c n)

{-# INLINE sum #-}
sum :: List Int -> Int
sum = foldl (+) 0

-- Look ma, no space leak!
-- And with foldl' there's still no space leak! Honest!
{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> List b -> a
foldl op e xs = foldr (\x y z -> y (z `op` x)) id xs e

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> List a -> b
foldr op e xs = foldDrop xs zero op e

{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = List $ \k c n ->
  foldDrop xs k (\x rest k ->
    foldDrop ys k (\y _ -> f x y `c` rest (succ k)) n) (const n) k

{-# INLINE toCh #-}
toCh xs = List $ \k op e -> myFoldr op e (myDrop k xs)

{-# INLINE myFoldr #-}
myFoldr op e = aux
  where aux [] = eta e
        aux (x:xs) = x `op` aux xs

{-# INLINE myDrop #-}
myDrop k xs = foldn k drop1 xs
  where
    drop1 _ (x:xs) = xs
    drop1 _ _ = []

{-# INLINE[0] eta #-}
eta x = x

{-# RULES
  "eta" forall x. eta x = \y -> eta (x y)
  #-}

{-# INLINE fromCh #-}
fromCh l = foldDrop l zero (:) []

{-# INLINE reverse #-}
reverse xs = foldl (flip cons) nil xs

{-# NOINLINE test #-}
test :: (a -> b) -> (b -> c) -> [a] -> [c]
test f g xs = fromCh (map g (map f (toCh xs)))

-- {-# NOINLINE test2 #-}
-- test2 :: [Int] -> [Int] -> Int
-- test2 xs ys = sum (toCh xs ++ toCh ys)

{-# NOINLINE test3 #-}
test3 :: [Int] -> Int
test3 xs = sum (map square (toCh xs))
  where square x = x * x

{-# NOINLINE test3a #-}
test3a :: [Int] -> Int
test3a xs = sum (zipWith (*) (toCh xs) (toCh xs))

{-# NOINLINE test4 #-}
test4 :: (a -> b) -> (b -> c) -> [a] -> [c]
test4 f g xs = fromCh (map g (tail (map f (toCh xs))))

-- {-# NOINLINE test5 #-}
-- test5 :: [a] -> [a] -> [a]
-- test5 xs ys = fromCh (toCh xs ++ (reverse (toCh ys) ++ toCh xs))
