#lang sweet-exp GDTL


(define
  (One : Nat)
  (One = (Succ Zero)))

(define
  (ConstNat : (-> Nat (Set 1)))
  (ConstNat x = Nat))



(define
  (plus : (-> Nat Nat Nat))
  ( plus m n = (NatElim m ConstNat n x rec (Succ rec))
         ))