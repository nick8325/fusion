{-# LANGUAGE RankNTypes, BangPatterns #-}

module Church(test, test3, test3a, test3b, test3c, test3d) where

import Prelude hiding (map, (++), foldl, sum, tail, zipWith, reverse, succ, pred, foldr, repeat, takeWhile, iterate)

type Church a = forall b c. (a -> (b -> c) -> (b -> c)) -> (b -> c) -> (b -> c)
newtype List a = List { foldZip :: forall b c. Church b -> Church (a, b) }

{-# INLINE nil #-}
nil = List $ \_ _ n -> n
-- {-# INLINE cons #-}
-- cons x xs = List $ \k c n ->
--   foldn k
--     (\m _ -> foldDrop xs m c n)
--     (c x (foldDrop xs zero c n))

{-# INLINE map #-}
map :: (a -> b) -> List a -> List b
map f xs = List $ \z c n e -> foldZip xs z (\(x, y) xs e -> c (f x, y) xs e) n e

-- {-# INLINE tail #-}
-- tail xs = List $ \k c n -> foldDrop xs (succ k) c n

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
foldl op e xs = fold (\x y z -> y (z `op` x)) id xs e

{-# INLINE fold #-}
fold :: (a -> (b -> c) -> (b -> c)) -> (b -> c) -> List a -> b -> c
fold op e xs = foldZip xs (\c n y -> repeatCh () c n y) (\(x, _) y e -> op x y e) e

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> List a -> b
foldr op e xs = fold (\x y z -> op x (y ())) (const e) xs ()

{-# INLINE repeatCh #-}
repeatCh :: a -> (a -> (b -> c) -> (b -> c)) -> (b -> c) -> (b -> c)
repeatCh x c n y = aux y
  where aux y = c x (\y -> aux y) y

{-# INLINE takeWhile #-}
takeWhile p xs = List $ \z c n e ->
  foldZip xs z (\(x, y) xs' e -> if p x then c (x, y) xs' e else n e) n e

{-# INLINE iterate' #-}
iterate' f x = List $ \z c n e ->
  let g x h (!y, e) = c (y, x) (\e -> h (f y, e)) e
  in z g (\(_, e) -> n e) (x, e)

{-# INLINE upto #-}
upto :: Int -> List Int
upto !m = takeWhile (<= m) numbers

{-# INLINE numbers #-}
numbers :: List Int
numbers = iterate' (+1) 0

{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = List $ \z c n e ->
  foldZip xs (foldZip ys z)
    (\(x, (y, z)) w e -> c (f x y, z) w e) n e

{-# INLINE toCh #-}
toCh :: [a] -> List a
toCh xs = List $ \z op e -> myFoldrZip z op e xs

{-# INLINE myFoldrZip #-}
myFoldrZip :: Church b -> ((a, b) -> c -> c) -> c -> [a] -> c
myFoldrZip z op e xs = z f (const e) xs
  where
    f x g [] = e
    f x g (y:ys) = (y, x) `op` g ys

-- {-# INLINE myDrop #-}
-- myDrop k xs = foldn k drop1 xs
--   where
--     drop1 _ (x:xs) = xs
--     drop1 _ _ = []

{-# INLINE fromCh #-}
fromCh l = foldr (:) [] l

-- {-# INLINE reverse #-}
-- reverse xs = foldl (flip cons) nil xs

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

{-# NOINLINE test3b #-}
test3b :: Int -> Int
test3b n = sum (map square (upto n))
  where square x = x * x

{-# NOINLINE test3c #-}
test3c :: Int -> Int
test3c n = sum (zipWith (*) (upto n) (upto n))

{-# NOINLINE test3d #-}
test3d :: Int -> Int
test3d n = sum (zipWith (*) (upto n) numbers)

-- {-# NOINLINE test4 #-}
-- test4 :: (a -> b) -> (b -> c) -> [a] -> [c]
-- test4 f g xs = fromCh (map g (tail (map f (toCh xs))))

-- {-# NOINLINE test5 #-}
-- test5 :: [a] -> [a] -> [a]
-- test5 xs ys = fromCh (toCh xs ++ (reverse (toCh ys) ++ toCh xs))
