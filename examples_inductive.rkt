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
  (head : {(A : Set(1)) (n : Nat) (Vec A (Succ n)) -> A })
  (head A n v = (VecElim v A (Succ n) (lambda (n2 v) (ifzero1 n2 Nat A)) 0 x1 x_head x3 x4 x_head )))

(define unsafeNil { (Nil Nat) :: (Vec Nat ?)})

(define unsafeCons {(Cons Nat 0 2 (Nil Nat)) :: (Vec Nat ?)})


;;Type-checks and runs successfully
(head Nat ? unsafeCons)

;;Type-checks but throws runtime exception
(head Nat ? unsafeNil)


