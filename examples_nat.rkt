#lang sweet-exp GDTL

;; Helper for defining the plus function
(define
  (ConstNat : (-> Nat (Set 1)))
  (ConstNat x = Nat))

;;We can eliminate over natural numbers to define iteration or perform induction
;;e.g. addition can be defined as follows:
(define
  (plus : (-> Nat Nat Nat))
  ( plus m n = (NatElim m ConstNat n m-1 rec (Succ rec))
         ))