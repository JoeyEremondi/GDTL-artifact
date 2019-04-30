#lang s-exp GDTL


;;We can eliminate over natural numbers to define iteration or perform induction
;;e.g. addition can be defined as follows:
(define
  (plus : (-> Nat Nat Nat))
  ( plus m n = (NatElim m (lambda (x) Nat) n m-1 rec (Succ rec))
         ))

;;We can even do large elimination, to choose a type from a number
;;This lets us define the safe head function
(define
  (ifzero1 : { -> Nat (Set 1) (Set 1) (Set 1) } )
  (ifzero1 n S1 S2 = (NatElim 2 n (lambda (x) (Set 1)) S1 x1 x2 S2)
           ))
;;We eliminate a vector, producing 0:Nat if it is empty,
;; and producing its first element otherwise
;; This is type-safe because the motive of our elimination is dependent on the length of the vector
(define
  (head : {-> (A : (Set 1)) (n : Nat) (Vec A (Succ n))  A })
  (head A n v = (VecElim v A (Succ n) (lambda (n2 v) (ifzero1 n2 Nat A)) 0 x1 x_head x3 x4 x_head )))

;; An empty vector with  length
(define safeNil {:: (Nil Nat)  (Vec Nat 0)})

;; A non-empty vector with known length
(define safeCons {:: (Cons Nat 0 2 (Nil Nat))  (Vec Nat 1)})

;; A vector with unknown length
(define unsafeNil {:: (Nil Nat)  (Vec Nat ?)})

;; Another vector with unknown length
(define unsafeCons {:: (Cons Nat ? 2 (Nil Nat))  (Vec Nat ?)})


;;Type-checks and returns 2
(head Nat 0 safeCons)



;;Does not typecheck
(head Nat ? safeNil)

;;Type-checks and returns 2
(head Nat ? unsafeCons)


;;Type-checks but throws runtime exception
(head Nat ? unsafeNil)


;; We can write normal static equalities
(define 1+1=2 {:: (Refl Nat 2)
                  (Eq Nat 2 (plus 1 1))})



;;And a generic substitution principle
(define
  (subst : (-> (A : (Set 1)) (P : (-> A  (Set 1))) (x : A) (y : A) (Eq A x y) (P x) (P y) ))
  (subst A P x y pf = (EqElim
                       pf
                       A
                       (lambda (x1 y1 pf1) (-> (P x1) (P y1)))
                       z (lambda (Pz) Pz)
                       x
                       y
                       )))

;; And statically they fail if the two terms aren't equal
;(define 1+1=1bad {:: (Refl Nat 1)
;                  (Eq Nat 1 (plus 1 1))})


;;But we can use gradual types to type absurd equalities
(define 0=1 {:: (Refl Nat ?)
                  (Eq Nat 0 1)})

(trace-on)

;; If we try to use an equality that doesn't dynamically hold, we get a runtime error
(::
 (subst
  Nat
  (lambda (n) (Vec Nat n))
  0
  1
  0=1
  (Nil Nat)
 )
 (Vec Nat 1))