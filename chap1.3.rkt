#lang planet neil/sicp

;; Exercise 1.29

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  ; add kernels of 1 4 1, 1 4 1, 1 4 1 centered on odd k
  (define step (+ (/ (- b a) n)))
  (define (next x) (+ x (* 2 step)))
  (define (term x) (+ (f (- x step)) 
                      (* 4 (f x)) 
                      (f (+ x step))))
  (*  (/ step 3) (sum term (+ step a) next b)))

(define (prob-1-29)
  (display (integral cube 0 1 1/100)) (newline)
  (display (simpson cube 0 1 100)) (newline)
  (display (integral cube 0 1 1/1000)) (newline)
  (display (simpson cube 0 1 1000)) (newline))

;; Exercise 1.30

;(define (sum term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (+ (term a) result)))) 
;  (iter a 0.0))

;; Exercise 1.31

;(define (product term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (* (term a) result)))) 
;  (iter a 1.0))

(define (factorial z)
  (product identity 1 inc z))

(define (pi-fourths-approx n)
  (define (term n) 
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (product term 1.0 inc n))

(define (product-rec term a next b)
  (if (> a b) 1
      (* (term a) (product-rec term (next a) next b))))

;; Exercise 1.32

(define (accumulate combiner init term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result)))) 
  (iter a init))

(define (sum term a next b) 
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; Exercise 1.33

(define (filtered-accumulate filter combiner term init a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
              (if (filter a)
                  (combiner (term a) result)
                  result))))
  (iter a init))

(define (sum-square-primes a b)
  (filtered-accumulate prime? + square 0 a inc b))

(define (sum-relative-primes n)
  (define (rel-prime-to-n x) (= (gcd n x) 1))
  (filtered-accumulate rel-prime-to-n + identity 0 1 inc n))

;; Exercise 1.34
(define (f g) (g 2))

;; (f f) => (f 2) => (2 2), which is an error as 2 is not an operator.

;; Exercise 1.35

(define (ff x) (+ 1 (/ 1 x)))

; Then (ff (/ (+ 1 (sqrt 5)) 2))
;=> (/ 1 (/ 1 (/ (+ 1 (sqrt 5)) 2)))
;=> (/ 1 (/ 2 (+ 1 (sqrt 5)))) since (/ 1 (/ x y)) => (/ y x)
;=> multiply numerator and denominator by (+ 1 (sqrt 5)):
;=> (/ (* 1 (+ 1 (sqrt 5))) (* (/ 2 (+ 1 (sqrt 5))) (+ 1 (sqrt 5))))
;=> (/ (+ 1 (sqrt 5)) 2))

(define (phi)
  (fixed-point 
   (lambda (x) (+ 1 (/ 1 x)))
   1.0))

;; Exercise 1.36

(define (fixed-point-trace f start)
  (display start) (newline)
  (define (trace-f x)
    (let ((result (f x)))
      (display result) (newline)
      result))
  (fixed-point trace-f start))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (ex-1-36)
  (define (f x) (/ (log 100) (log x)))
  (fixed-point-trace f 2)
  (fixed-point-trace (average-damp f) 2))

; Without damping, 54 steps; with 8.

;; Exercise 1.37
(define (cont-frac n d k)
  (define (inner i)
    (if (> i k) 1
        (/ (n i) (+ (d i) (inner (inc i))))))
  (inner 1.0))

(define (phi-approx k)
  (cont-frac-iter (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))
;; I get 0.6180... with k=11

(define (cont-frac-iter n d k)
  ; this computed from the inside out...
  (define (iter i accum)
    (if (< i 1)
        accum
        (iter (dec i) (/ (n i) (+ (d i) accum)))))
  (iter k 1.0))

;; Exercise 1.38
(define (e-2 k) 
  (define (n i) 1)
  (define (d i) 
    (if (= (remainder i 3) 2)
        (* 2 (/ (+ i 1) 3))
        1))
  (cont-frac-iter n d k))

;; Exercise 1.39
(define (tan-cf x k)
  (define (n i) (if (> i 1) (- (square x)) x))
  (define (d i) (dec (* 2 i)))
  (cont-frac-iter n d k))

;; Exercise 1.40

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

;; Exercise 1.41

(define (double f) (lambda (x) (f (f x))))

; ((double (double double)) inc) increments 16 times.

;; Exercise 1.42

(define (compose f g) (lambda (x) (f (g x))))

;; Exercise 1.43

(define (repeated f k)
  (if (= k 0)
      identity
      (lambda (x) ((repeated f (dec k)) (f x)))))
;; is it possible to define repeated/compose in a 
;; way that generates an iterative process?

;; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ 3 (+ (f (+ x dx)) (f x) (f (+ x dx))))))

(define (smooth-r f n) ((repeated smooth n) f))

;; Exercise 1.45

(define (fixer n x) (lambda (y) (/ x (pow y (dec n)))))

; two dampings works through 7th roots, not 8
; three smoothings works through 15th roots, not 16
; so we need floor log2 n dampings.... plus one?
(define (floor-log2 x) 
  (if (<= x 2) 1 (+ 1 (floor-log2 (/ x 2)))))
; don't care that it's recursive because 'repeated' 
; generates a recursive procedure

(define (root n x)
  (fixed-point 
    ((repeated average-damp (floor-log2 x)) 
      (fixer n x))
    1.0))

;; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (define (iter x)
    (if (good-enough? x) x (iter (improve x))))
  iter)

(define (sqrt x)
  ((iterative-improve 
    (lambda (y) (< (abs (- (square y) x)) tolerance))
    (lambda (y) (/ (+ y (/ x y)) 2))) x))

(define (fixed-point f x)
  ((iterative-improve
    (lambda (x) (< (abs (- x (f x))) tolerance)) ;;hmm, duplicates work
    f) 
   x))

;; ---- defined in text

(define tolerance 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; ------- extra definitions needed from earlier problem sets

(define (pow b n)
  (pow-iter b n 1))

(define (pow-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (pow-iter (square b) (/ n 2) a))
        (else (pow-iter b (dec n) (* a b)))))


(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
      (= n (smallest-divisor n)))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (find-divisor n test-divisor)
  (define (next n) (if (odd? n) (+ 2 n) (inc n)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))