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


(define
  (fold : {(A : Set(1)) (B : Set(1)) ( n : Nat) {A B -> B} B (Vec A n) -> B})
  (fold A B n f z v = (VecElim
                       v
                       A
                       n
                       (lambda (x y) B)
                       z
                       x1
                       a
                       x3
                       b
                       (f a b)
                       )))

