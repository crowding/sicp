#lang planet neil/sicp

;; Exercise 1.6

;; It will recurse endlessly, as it makes a recursive
;; call before it checks whether to halt.

;; Exercize 1.7 
(define (sqrt x)
      (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
      (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
(define (improve guess x)
      (average guess (/ x guess)))
(define (square x) (* x x))
(define (average x y)
      (/ (+ x y) 2))
(define (good-enough? guess x)
      (<= (abs (- (square guess) x)) (* x 0.00001)))

;; with unrestricted real numbers, this is more accurate for smaller
;; numbers but less for larger numbers (but in practice is a better match to
;; floating point precision for all size numbers)

;; Exercise 1.8

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
   3))
(define (cbrt x) (cbrt-iter 1.0 x))
(define (good-enough-cbrt? guess x)
  (<= (abs (- (* guess guess guess) x)) (* x 1e-12)))
(define (cbrt-iter guess x) 
  (if (good-enough-cbrt? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x) x)))

;; Exercise 1.9
(define (ex-1-9)
  (define (+ a b) (if (= a 0) b (inc (+ (dec a) b))))
  ;expands like:
  (+ 4 5)
  (inc (+ 3 5))
  (inc (inc (+ 2 5)))
  (inc (inc (inc (+ 1 5))))
  (inc (inc (inc (inc (+ 0 5)))))
  (inc (inc (inc (inc 5))))
  (inc (inc (inc 6)))
  (inc (inc 7))
  (inc 8)
  9)

(define (ex-1-9-b)
  (define (+ a b) (if (= a 0) b (+ (dec a) (inc b))))
  ;expands like:
  (+ 4 5)
  (+ 3 6)
  (+ 2 7)
  (+ 1 8)
  (+ 0 9)
  9)

;;Exercise 1.10
(define (ex-1-10)
  (define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))))
  
  (A 1 10)
  (A 0 (A 1 9))
  (A 0 (A 0 (A 1 8)))
  ;...
  (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1))))))))))
  (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
  ;...
  1024 ;; generalizing, (A 1 x) => 2^x.
  
  (A 2 4)
  (A 1 (A 1 (A 1 (A 1 1))))
  (A 1 (A 1 (A 1 2)))
  (A 1 (A 1 4))
  (A 1 16)
  65536 ;; generalizing, (A 2 4) => 2^2^2^2, call it 2 ^^ 4.
  
  (A 3 3)
  (A 2 (A 2 (A 2 1)))
  (A 2 (A 2 2))
  (A 2 4)
  65536 ;; generalizing, (A 3 3) => 2 ^^ 2 ^^ 2, call it 2 ^^^ 3
  
  ; (f n) => 2*n or +_1^N 2
  ; (g n) => 2^n or *_1^N 2
  ; (h n) => 2 ^^ n or ^_1^N 2 (where ^^ n means "repeated exponent")
  )
;; Exercise 1.11

(define (frec n)
  (cond ((< n 3) n)
        (else (+ (frec (dec n))
                 (* 2 (frec (- n 2)))
                 (* 3 (frec (- n 3)))))))

(define (fit n)
  (fit-iter 0 1 2 (- n 2)))

(define (fit-iter fn-3 fn-2 fn-1 i)
  (if (<= i 0) (+ i fn-1) ; (fit 0) => 0, (fit 1) => 1 etc
      (fit-iter fn-2 
                fn-1 
                (+ fn-1 (* 2 fn-2) (* 3 fn-3))
                (dec i))))
  
;(fit 3)
;(fit-iter 0 1 2 1)
;(fit-iter 1 2 4 0)

;; Exercise 1.12

(define (pascal row ix)
  (cond ((<= row 1) 1)
        ((<= ix 1) (pascal (dec row) ix))
        ((>= ix row) (pascal (dec row) (dec ix)))
        (else (+ (pascal (dec row) ix) 
                 (pascal (dec row) (dec ix))))))


;; Exercise 1.13
  
;plug and chug algebra elided. Just plug the given values 
;into the recurrence, one finds that they satisfy f(n) = f(n-1) + f(n-2)
;with f(0) = 0 and f(1) = 1. One realizes that the fibonnacci sequence with 
;other starting points amounts to different scaling constant plus different 
;value of psi (while phi is fixed and psi < phi).

;; Exercise 1.14

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
 
;; On expanding this, one discovers that it is O(amount) in space (because 
;; you can always count out pennies, and its depth is the number of 
;; coins counted out in the worst case. For similar reasons, I think the 
;; time taken is O(N^2) (consider: count out all pennies; count out one
;; nickel and pennies; count out two nickels and pennies, etc.)

;; Exercise 1.15

; A. Five times: (/ 12.5 3 3 3 3 3) => 0.05
; B. O(log a)

;; Exercise 1.16

(define (it-exp b n)
  (it-exp-iter b n 1))

(define (it-exp-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (it-exp-iter (square b) (/ n 2) a))
        (else (it-exp-iter b (dec n) (* a b)))))
       
;;Exercise 1.1.7

(define (mult a b)
  (mult-iter a b 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult-iter a b x)
  (cond ((= b 0) x)
        ((even? b) (mult-iter (double a) (halve b) x))
        (else (mult-iter a (dec b) (+ x a)))))

;;Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; compute p’
                   (+ (* 2 p q) (square q)) ; compute q’ 
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; Exercise 1.20


;applicative order...
(define (ex-1-20)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (gcd 206 40)
  (gcd 40 (remainder 206 40))
  (gcd 40 6)
  (gcd 6 (remainder 40 6))
  (gcd 6 4)
  (gcd 4 (remainder 6 4))
  (gcd 4 2)
  (gcd 2 (remainder 4 2))
  (gcd 2 0)
  2
  )

; I don't see why the order of operations should differ here between 
; normal and applicative order....
; unless normal order is unlazy, which doesn't fit the substitution model.
; Which it seems is the point. I don't want to hand expand it. Okay, so by 
; "normal order" they really do mean "macro style"

;; Exercise 1.21

(define (smallest-divisor n) (find-divisor n 2))
;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
      (= n (smallest-divisor n)))

(define (ex-1-21)
  (smallest-divisor 199) ;199
  (smallest-divisor 1999);1999
  (smallest-divisor 19999));7

;; Exercise 1.22
  
(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))
;(define (start-prime-test n start-time)
;  (cond ((prime? n)
;         (report-prime (- (runtime) start-time)) #t)
;        (else #f)))
(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

(define (search-for-primes lower howmany)
  (define (iter lower howmany accum)
    (cond ((= 0 howmany) accum)
          ((timed-prime-test lower) 
           (iter (+ 2 lower) (dec howmany) (cons lower accum)))
          (else (iter (+ 2 lower) howmany accum))))
  (iter (if (odd? lower) lower (inc lower)) howmany '()))
; 100 -> 12us, 10000 -> 30 us, 100000 -> 85 us, 1000000 -> 170 us
; the runtimes seem to diagnose an O(sqrt(n)) runtime for prime checking

;; exercise 1.23
; (search-for-primes 10000000 3) ~ 1300 us per prime before modification
(define (next n) (if (odd? n) (+ 2 n) (inc n)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
;after modification, ~ 800 us per test, so prime testing 
;could take up 1000 ms and been cut in half, with 300us other 
;overhead (output?)

;; exercise 1.24

(define (expmod base exp m)
  (cond ((= exp 0)
         1)
        ((even? exp)
         (remainder
          (square
           (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base
             (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
      (cond ((= times 0) true)
            ((fermat-test n) (fast-prime? n (- times 1)))
            (else false)))
(define (start-prime-test n start-time)
  (cond ((fast-prime? n 10)
         (report-prime (- (runtime) start-time)) #t)
        (else #f)))

;; this appears to be closer to an O(log n) runtime.

;; Exercise 1.25

; Dealing with large integers takes at least O(log n) for all operations,
; so when this calls (fast-expt base exp) it is creating a runtime of 
; at least O(exp) rather than the O(log exp) of the modular exponentiation
; algorithm.

;; Exercise 1.26

;; He's making an dyadic call tree of O(log n) depth, by calling expmod
;; twice from expmod.

;; Exercise 1.27

(define (carmichael-check n) 
  (define (carmichael-iter a n)
    (if (= a n) #t
        (and (= a (expmod a n n))
             (carmichael-iter (inc a) n))))
  (carmichael-iter 2 n))
               
(define carmichael-numbers (list 561 1105 1729 2465 2821 6601))
(define (ex-1-26) (map fermat-test carmichael-numbers))

;;Exercise 1.28

(define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) 
         (let* ((mm (expmod-mr base (/ exp 2) m))
                (r (remainder (square mm) m)))                            
           (if (and (= r 1) ;nontrivial means nontrivial mod m
                    (> mm 1) 
                    (< mm (dec m))) 
               0 r)))
        (else (remainder (* base (expmod-mr base (- exp 1) m)) m))))
    
(define (mr-test n)
  (define (try-it a)
    (let ((em (expmod-mr a (dec n) n)))
      (if (= em 0) #f
          (= em 1))))
  (try-it (+ 1 (random (- n 1)))))
  
(define (mr-prime? n times)
   (cond ((= times 0) #t)
         ((mr-test n) (mr-prime? n (- times 1)))
         (else #f)))
    
(define (mr-demo)
  (define (check? n) (mr-prime? n 20))
  (display (map check? carmichael-numbers)) ;all false
  (display (map check? (search-for-primes 1000000 10)))) ;all true
