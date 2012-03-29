
{-# OPTIONS -Wall #-}

{-# LANGUAGE 
MultiParamTypeClasses,
DataKinds,
KindSignatures,
GADTs, 
TypeFamilies #-}

module Data.Annotated.Annotations where

data N = Z | S N

{-
data LEQ_Pf :: N -> N -> * where
  LEQ_Z :: LEQ_Pf Z n
  LEQ_S :: LEQ_Pf n1 n2 -> LEQ_Pf (S n1) (S n2)
-}

forbidden :: a
forbidden = error "Type system forbids this annotated pattern"

{-
class LEQ (n1 :: N) (n2 :: N) where
  proof :: LEQ_Pf n1 n2

instance LEQ Z (S n) where
  proof = LEQ_Z

instance LEQ n1 n2 => LEQ (S n1) (S n2) where
  proof = LEQ_S proof
-}

type family ADD (n1 :: N) (n2 :: N) :: N
type instance ADD Z n = n
type instance ADD (S n1) n2 = S (ADD n1 n2)
