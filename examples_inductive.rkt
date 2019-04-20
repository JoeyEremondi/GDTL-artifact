#lang sweet-exp GDTL

;;Syntax is using Racket's "sweet expressions",
;;an extension of s-expressions
;;https://docs.racket-lang.org/sweet/index.html

;;We can eliminate over natural numbers to define iteration or perform induction
;;e.g. addition can be defined as follows:
(define
  (plus : (-> Nat Nat Nat))
  ( plus m n = (NatElim m (lambda (x) Nat) n m-1 rec (Succ rec))
         ))

;;We can even do large elimination, to choose a type from a number
;;This lets us define the safe head function
(define
  (ifzero1 : { Nat Set(1) Set(1) -> Set(1) } )
  (ifzero1 n S1 S2 = (NatElim 2 n (lambda (x) Set(1)) S1 x1 x2 S2)
                            ))
;;We eliminate a vector, producing 0:Nat if it is empty,
;; and producing its first element otherwise
;; This is type-safe because the motive of our elimination is dependent on the length of the vector
(define
  (head : {(A : Set(1)) (n : Nat) (Vec A (Succ n)) -> A })
  (head A n v = (VecElim v A (Succ n) (lambda (n2 v) (ifzero1 n2 Nat A)) 0 x1 x_head x3 x4 x_head )))

;; An empty vector with  length
(define safeNil { (Nil Nat) :: (Vec Nat 0)})

;; A non-empty vector with known length
(define safeCons {(Cons Nat 0 2 (Nil Nat)) :: (Vec Nat 1)})

;; A vector with unknown length
(define unsafeNil { (Nil Nat) :: (Vec Nat ?)})

;; Another vector with unknown length
(define unsafeCons {(Cons Nat ? 2 (Nil Nat)) :: (Vec Nat ?)})

;;Type-checks and runs successfully
(head Nat 0 safeCons)

;;Does not typecheck
;(head Nat ? safeNil)

;;Type-checks and runs successfully
(head Nat ? unsafeCons)

;;Type-checks but throws runtime exception
(head Nat ? unsafeNil)


