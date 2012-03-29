
{-# OPTIONS -Wall #-}

{-# LANGUAGE DataKinds,
             KindSignatures,
             GADTs,
             FlexibleInstances,
             FlexibleContexts,
             ScopedTypeVariables #-}

module Data.Annotated.LengthIndexedVectors 
       (
         LIV, length, unLIV, null, (!!), replace,
         head, last, tail, init, take, drop, select,
         (++), map, zipWith, foldl, foldr, iterate,
         copy
       ) where

import Prelude hiding 
  (length, null, (!!), head, last, tail, init, take, 
   drop, (++), map, zipWith, foldl, foldr, iterate)
  
import qualified Data.Foldable as DF

import Data.Annotated.Annotations
import Data.Annotated.Indices

-- | GADT for lists with length annotations of the
-- natural numbers

data LIV :: N -> * -> * where
  Nil  :: LIV Z a
  Cons :: a -> LIV n a -> LIV (S n) a

-- | Common type classes

instance Eq (LIV Z a) where
  Nil == Nil = True

instance (Eq a, Eq (LIV n a)) => Eq (LIV (S n) a) where
  Cons x xs == Cons y ys = x == y && xs == ys 

instance Show a => Show (LIV n a) where
  show = show.unLIV

-- | Common functions

length :: LIV n a -> Int
length Nil = 0
length (Cons _ xs) = 1 + length xs

unLIV :: LIV n a -> [a]
unLIV Nil         = []
unLIV (Cons x xs) = x : unLIV xs

null :: LIV Z a -> Bool
null Nil = True

(!!) :: Index n0 n -> LIV n a -> a
(!!) Zero     (Cons x _)  = x
(!!) (Succ n) (Cons _ xs) = n !! xs
(!!) _        Nil          = forbidden

replace :: Index n0 n -> a -> LIV n a -> LIV n a
replace Zero     x (Cons _ ys) = Cons x ys
replace (Succ n) x (Cons y ys) = Cons y $ replace n x ys
replace _        _ Nil         = forbidden

head :: LIV (S n) a -> a
head (Cons x _) = x

last :: LIV (S n) a -> a
last (Cons x Nil) = x
last (Cons _ xs@(Cons _ _)) = last xs

tail :: LIV (S n) a -> LIV n a
tail (Cons _ xs) = xs

init :: LIV (S n) a -> LIV n a
init (Cons _ Nil) = Nil
init (Cons x xs@(Cons _ _)) = Cons x $ init xs

-- n1 is the N of elements in the dest
-- n2 is the minimum required N in the source, n2 >= n1
take :: Index n1 (S n2) -> LIV n2 a -> LIV n1 a
take Zero _ = Nil
take (Succ n) (Cons x xs) = Cons x $ take n xs
take (Succ _) _ = forbidden

-- n1 is the N of elements dropped (excluded)
-- n2 is the N of elements remaining
-- The source must have had exactly (n1+n2) elements
drop :: Index n1 _n -> LIV (ADD n1 n2) a -> LIV n2 a
drop Zero     liv         = liv
drop (Succ n) (Cons _ xs) = drop n xs
drop (Succ _) _           = forbidden

-- n1 is the N of elements dropped
-- n2 is the N of elements taken, and the N in the dest, n3 >= n2
-- The source must have had >= (n1+n2) elements
select :: Index n1 _n -> Index n2 (S n3) -> LIV (ADD n1 n3) a -> LIV n2 a
select n1 n2 = take n2 . drop n1

(++) :: LIV n1 a -> LIV n2 a -> LIV (ADD n1 n2) a
(++) Nil liv = liv
(++) (Cons x xs) liv = Cons x $ xs ++ liv

map :: (a -> b) -> LIV n a -> LIV n b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) $ map f xs

zipWith :: (a -> b -> c) -> LIV n a -> LIV n b -> LIV n c
zipWith _ Nil Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = 
  Cons (f x y) $ zipWith f xs ys
zipWith _ _ _ = forbidden

foldl :: a -> (a -> b -> a) -> LIV n b -> a
foldl z _ Nil = z
foldl z f (Cons x xs) = foldl (f z x) f xs

foldr :: (a -> b -> b) -> b -> LIV n a -> b
foldr _ z Nil = z
foldr f z (Cons x xs) = f x $ foldr f z xs

iterate :: Index n n0 -> (a -> a) -> a -> LIV n a
iterate Zero _ _  = Nil
iterate (Succ n) f x = Cons x (iterate n f (f x))

copy :: Index n n0 -> a -> LIV n a
copy Zero _ = Nil
copy (Succ n) x = Cons x $ copy n x

instance DF.Foldable (LIV n) where
  foldr = foldr

instance Functor (LIV n) where
  fmap = map

