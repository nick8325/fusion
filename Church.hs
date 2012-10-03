{-# LANGUAGE RankNTypes #-}

module Church(test, test3, test3a, test3b, test3c) where

import Prelude hiding (map, (++), foldl, sum, tail, zipWith, reverse, succ, pred, foldr, repeat)

type Church a = forall b. (a -> b -> b) -> b -> b
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
map f xs = List $ \z c n -> foldZip xs z (\(x, y) xs -> (f x, y) `c` xs) n

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
foldl op e xs = foldr (\x y z -> y (z `op` x)) id xs e

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> List a -> b
foldr op e xs = foldZip xs (repeatCh ()) (\(x, _) y -> op x y) e

{-# INLINE[0] repeatCh #-}
repeatCh :: a -> (a -> b -> b) -> b -> b
repeatCh x c n = aux
  where aux = c x aux

{-# RULES
"repeatCh" repeatCh = repeatCh1
"repeatCh1" repeatCh1 = repeatCh2
"repeatCh2" repeatCh2 = repeatCh3
  #-}

{-# INLINE[0] repeatCh1 #-}
repeatCh1 :: a -> (a -> (b -> c) -> (b -> c)) -> (b -> c) -> (b -> c)
repeatCh1 x c n = \y -> loop y
  where loop = \y -> c x (\x -> loop x) y

{-# INLINE[0] repeatCh2 #-}
repeatCh2 :: a -> (a -> (b -> c -> d) -> (b -> c -> d)) -> (b -> c -> d) -> (b -> c -> d)
repeatCh2 x c n = \y z -> monkey y z
  where monkey = \y z -> c x (\x y -> monkey x y) y z

{-# INLINE[0] repeatCh3 #-}
repeatCh3 :: a -> (a -> (b -> c -> d -> e) -> (b -> c -> d -> e)) -> (b -> c -> d -> e) -> (b -> c -> d -> e)
repeatCh3 x c n = \y z w -> elephant y z w
  where elephant = \y z w -> c x (\x y z -> elephant x y z) y z w

{-# INLINE unfoldr #-}
unfoldr :: (b -> Maybe (a, b)) -> b -> List a
unfoldr f s = List $ \z c n ->
  let g x h s =
        case f s of
          Nothing -> n
          Just (y, s') -> (y, x) `c` h s'
  in z g (const n) s

{-# INLINE upto #-}
upto :: Int -> List Int
upto m = unfoldr f 0
  where f n | n >= m = Nothing
            | otherwise = Just (n, n+1)

{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = List $ \z c n ->
  foldZip xs (foldZip ys z)
    (\(x, (y, z)) w -> (f x y, z) `c` w) n

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

{-# INLINE[0] eta #-}
eta x = x

{-# RULES
  "eta" forall x. eta x = \y -> eta (x y)
  #-}

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

-- {-# NOINLINE test4 #-}
-- test4 :: (a -> b) -> (b -> c) -> [a] -> [c]
-- test4 f g xs = fromCh (map g (tail (map f (toCh xs))))

-- {-# NOINLINE test5 #-}
-- test5 :: [a] -> [a] -> [a]
-- test5 xs ys = fromCh (toCh xs ++ (reverse (toCh ys) ++ toCh xs))
