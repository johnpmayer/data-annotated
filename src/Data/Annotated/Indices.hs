
{-# OPTIONS -Wall #-}

{-# LANGUAGE
DataKinds,
GADTs, 
KindSignatures #-}

module Data.Annotated.Indices where

import Data.Annotated.Annotations

-- | The index type is annotated by two natural numbers
-- 1) The unique corresponding value
-- 2) A value corresponding to a finite set containing this index

data Index :: N -> N -> * where
  Zero :: Index Z (S n)
  Succ :: Index val fis -> Index (S val) (S fis)

