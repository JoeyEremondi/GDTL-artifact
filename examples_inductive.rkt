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
  (constSet1 : {Nat -> Set(2)})
  (constSet1 x = Set(1))
  )

(define
  (ifzero1 : { Nat Set(1) Set(1) -> Set(1) } )
  (ifzero1 n S1 S2 = (NatElim 2 n (lambda (x) Set(1)) S1 x1 x2 S2)
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


(define
  (head : {(A : Set(1)) (n : Nat) (Vec A (Succ n)) -> A })
  (head A n v = (VecElim v A (Succ n) (lambda (n2 v) (ifzero1 n2 Nat A)) 0 x1 x_head x3 x4 x_head )))

(define unsafeNil { (Nil Nat) :: (Vec Nat ?)})

(define unsafeCons {(Cons Nat 0 99 (Nil Nat)) :: (Vec Nat ?)})

(trace-on)

;;Type-checks and runs successfully
(head unsafeCons)

;;Type-checks but throws runtime exception
(head unsafeNil)
