#lang planet neil/sicp

;;; Exercise 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))



;;; Exercise 3.51

;; The first call to stream-ref will print 1 through 6. The second will
;; print 7 and 8, having already memoized the first.

(define (show x)
  (display-line x)
  x)



;;; Exercise 3.52

;; (define sum 0)
                                     ; 0
;; (define (accum x) (set! sum (+ x sum)) sum)
                                     ; 0
;; (define seq
;;   (stream-map accum
;;               (stream-enumerate-interval 1 20)))

;;         sum is 1 (because stream-map forces the first item of the stream)
;;         seq has: 1 || 3 6 10 15 21 28 36 45 ...}
;;
;;         where || divides evaluated from not-yet-evaluated.

;; (define y (stream-filter even? seq))

;;         sum is 6
;;         seq has: 1 3 6 || 10 15 21 28 36 45 55 66
;;                           78 91 105 120 136 153 171 190 210
;;         y has: 6 || 10 28 36 66 78 120 136...

;; (define z
;;   (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                  seq))

;;         sum is 10
;;         z has: 10  || 15 45 55 105 120 170...

;; (stream-ref y 7)

;;         sum is now 136
;;         y now has 6 10 28 36 66 78 120 136 || ...

;; (display-stream z)
;;         sum is now 210 (complete)

;; These values would differ if streams were not memoized; generally
;; being larger as "seq" would keep being recomputed, adding to "sum"

;; Lazy evaluation with side effects bad; nonlazy evaluation with
;; side effects worse?


;;; Exercise 3.53

;; Simple substitution shows the pattern, this stream produces the
;; powers of 2.

;; S is {1 | (add-streams S S) ...}
;; {1 (+ 1 1) (add-streams (cdr S) (cdr S))}
;; {1 2 4 ...}



;;; Exercise 3.54

(define (mul-streams . streams)
  (if (stream-null? (stream-car (car streams)))
      'the-empty-stream
      (cons-stream
       (apply * (map stream-car streams))
       (apply mul-streams (map stream-cdr streams)))))

(define factorials
  (cons-stream
   1
   (mul-streams factorials
                (integers-starting-from 2))))



;;; Exercise 3.55

(define (accumulate-stream op initial stream)
  (if (stream-null? stream) 'the-empty-stream
      (let ((result (op initial (stream-car stream))))
        (cons-stream result
                     (accumulate-stream
                      op result (stream-cdr stream))))))

(define (partial-sums stream) (accumulate-stream + 0 stream))

(define (demo-55)
  (display-head (partial-sums integers) 5))



;;; Exercise 3.56

(define (scale-stream S factor)
  (stream-map (lambda (x) (* x factor)) S))

(define hamming (cons-stream 1 (merge (scale-stream hamming 2)
                                      (merge (scale-stream hamming 3)
                                             (scale-stream hamming 5)))))



;;; Exercise 3.57

;;; Taking this iomplementation of Fibbonacci:
(define (add-streams . s)
  (apply stream-map (cons + s)))

(define fibs
  (cons-stream
   1 (cons-stream
      1 (add-streams fibs (stream-cdr fibs)))))

;; Memoization gives n-1 additions to find the nth Fibbonacci
;; number.
;;
;; Define  Fn as the object you get from taking n successive cdrs of F
;; and Tn is the object you get from (stream-ref F 2)
;;
;; Doing substitution, I find the sstream expands as:
;;
;; F = (1 1 ... (add-streams F F1)
;;; (cadr F) is known, no additions for n=0 or n=1
;; F = (1 1 (+ T0 T1) ... (add-streams F2 F3))
;;; at n=2 there is one addition to find (caddr F)
;; F = (1 1 2 (+ T1 T2) ... (add-streams F3 F4))
;; F = (1 1 2 3 (+ T2 T3) ... (add-streams F4 F5))
;;; We can see that at each successive N, one addition is performed.

;; Now, what happens when the stream nodes are not memoized is that
;; when we take stream-cdr of Fn, top give F(n+1) you have to recompute Tn+1.
;; Let An be the number of additions to reach Fn. Then...

;; F0 = (1 1 ... (add-streams F0 (cdr F0)))
;; F1 = (1 ... (add-streams F0 F1))
;;; no additions to comptue F1/T1
;; F2 = ((+ (car F0) (car F1)) ... (add-streams (cdr F0) (cdr F1)))
;;; one addition to compute F2/T2
;; F3 = ((+ (car F1) (car F2)) ... (add-streams (cdr F1) (cdr F2)))
;; computing T3 is one addition plus computing T2 plus computing T1

;;; Then An = A(n-1) + A(n-2) + 1, with T0 = 0 and T1 = 0.
;;; We can implement this function:

(define (const-stream C) (cons-stream C (const-stream C)))

(define a (cons-stream
           0 (cons-stream
              0 (add-streams
                 a
                 (stream-cdr a)
                 (const-stream 1)))))

;;; More importantly, we can see that A is monotonically increasing, so
;;; A(n+2) > 2 * An, which means that A grows at least exponentially.



;;; Exercise 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;;; Exercising some more substitution...

;; (expand 1 7 10)
;; = {(quotient (* 1 10) 7) (expand (remainder (* 1 10) 7) 7 10)}
;; = {1 . (expand 3 7 10)}
;; = {1 4 . (expand 2 7 10)}
;; = {1 4 2 . (expand 6 7 10)}
;; = {1 4 2 8 . (expand 4 7 10)}
;; = {1 4 2 8 5 . (expand 5 7 10)}
;; = {1 4 2 8 5 7 . (expand 1 7 10)}
;; = {1 4 2 8 5 7 1 4 2 8 5 7 (expand 1 7 10)}

;;; This computes a decimal expansion of 1/7 ---
;;; (1 4 2 8 5 7 1 4 2 8 5 7 ...) repeating.

;;; Similarly, (expand 3 8 10) computes
;;; (3 7 5 0 0 0 0 0 0 0 0 0 ...)



;;; Exercise 3.59

;; A)
(define (integrate-term coef exponent) (/ coef exponent))
(define (integrate-series S)
  (stream-map integrate-term S (integers-starting-from 1)))

;;B)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))
(define sine-series
  (cons-stream 0 (scale-stream (integrate-series cosine-series) -1)))



;;; Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

(define (demo-60)
  (display-head (add-streams
                 (mul-series sine-series sine-series)
                 (mul-series cosine-series cosine-series))
                10))




;;; Exercise 3.61

(define (invert-unit-series S)
  (define X (cons-stream 1 (scale-stream (mul-series (stream-cdr S) X) -1)))
  (if (= 1 (stream-car S))
      X
      (error "Not a unit series")))



;;; Exercise 3.62

;; S1/S2 = S1/(U*NS2) = (S1*(1/U)) * I(NS2)

(define (div-series s1 s2)
  ;; what, consecutive defines do not turn into a letrec?
  ;; (define s2-unit (stream-car s2))
  ;; (define s2-norm (scale-stream s2 (/ 1 s2-unit)))
  (let ((s2-unit (stream-car s2)))
    (let ((s2-norm (scale-stream s2 (/ 1 s2-unit))))
      (mul-series (scale-stream s1 (/ 1 s2-unit))
                  (invert-unit-series s2-norm)))))

;; tangent is sin over cosine. There are staging problems with
;; order of definitions so this is a function
(define (tangent-series)
  (div-series sine-series cosine-series))



;;; Exercise 3.63

;; In the version that Louis proposes, the call to (sqrt-stream)
;; creates a new stream object, and is not a reference to the stream
;; presently being created. This requires the environment rather than
;; substitution model to see.




;;; Exercise 3.64

(define (stream-limit s tol)
  (let ( (a (stream-car s))
         (b (stream-car (stream-cdr s))))
    (if (< (abs (- a b)) tol)
        b
        (stream-limit (stream-cdr s) tol))))

(define (sqrt-tol x tolerance)
  (stream-limit (sqrt-stream x) tolerance))



;;; Exercise 3.65

(define (ln2-summands n)
  (cons-stream (/ 1.0 n) (stream-map - (ln2-summands (+ n 2)))))
(define (ln2-stream) (partial-sums (ln2-summands 1)))

(define (demo-65)
  (let ((stream1 (ln2-stream)))
    (define stream2 (euler-transform stream1))
    (define stream3 (accelerated-sequence euler-transform stream1))

    (display-head (stream-map list stream1 stream2 stream3) 20)))




;;; Exercise 3.66

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave x y)
  (cons-stream (stream-car x) (interleave y (stream-cdr x))))

;;; Note that the first (i,j) will be presented before (i+1, j)
;;; (i,j) will be presented before (i, j+1).
;;;
;;; It seems also that (i, x) for all x comes up about twice as
;;; frequently as (i+1, y) for all y
;;;
;;; The first (i,x) pair to appear will be (i,i). It will appear on
;;; the J(i)'th term, where J(i) = 2*(J(i-1) + 1). J={0,2,6,14, ...} = 2^i-2
;;; which simplifies to J(i) = 2^i-2
;;;
;;; Then the term (i, x+1) occurs 2^(i-1) terms later, after which
;;; successive (i,x) pairs with increasing x will appear once every
;;; 2^(i) terms.

(define (term-predict i j)
  (cond
   ((= i j) (- (expt 2 i) 2))
   ((= (+ 1 i) j) (+ (expt 2 (- i 1)) (term-predict i (- j 1))))
   ((< (+ 1 i) j) (+ (expt 2 i) (term-predict i (- j 1))))
   (else (error "something went wrong" (list i j)))))

;;; So term (i, j) where j >= 2+i occurs at the term numbered Tij
;;;   2^i - 2 + 2^(i-1) + (j-1-i)*(2^i)
;;; = -2 + 2^(i-1) + (j-i)*(2^i)

;;; So (1, 100) appears at term number -2 + 1 + 99*2 = 197
;;; Term (99, 100) appears at index (2^99-2)+2^98
;;; while (100, 100) appears at index (2^100)-2



;;; Exercise 3.67

;; (a0, b0) | (a0, b1) (a0, b2) ...
;; ---------+---------------------
;; (a1, b0) | (a1, b1) (a0, b2)
;; (a2, b0) | (b2, b1) (a1, b2)

(define (interleave-3 x y z)
  (cons-stream (stream-car x) (interleave-3 y z (stream-cdr x))))

(define (all-pairs a b)
  (cons-stream (list (stream-car a) (stream-car b))
               (interleave-3
                (stream-map list
                            (const-stream (stream-car a))
                            (stream-cdr b))
                (stream-map list
                            (stream-cdr a)
                            (const-stream (stream-car b)))
                (all-pairs (stream-cdr a) (stream-cdr b)))))


;;; Exercise 3.68

;;; You can't directly recurse in setting up the interleave, as you
;;; loop indefinitely.  You have to put the recursive call in a
;;; stream-cdr, or otherwise delay its execution.

(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (louis-pairs (stream-cdr s) (stream-cdr t))))



;;; Exercise 3.69

(define (triples i j k)
  (cons-stream (list (stream-car i) (stream-car j) (stream-car k))
               (interleave-3
                (stream-map list
                            (const-stream (stream-car i))
                            (const-stream (stream-car j))
                            (stream-cdr k))
                (stream-map cons
                            (const-stream (stream-car i))
                            (pairs (stream-cdr j) (stream-cdr k)))
                (triples (stream-cdr i) (stream-cdr j) (stream-cdr k)))))

(define (pythagorean-triples)
  (stream-filter
   (lambda (l)
     (let ((a (car l))
           (b (cadr l))
           (c (caddr l)))
       (= (+ (square a) (square b)) (square c))))
   (triples integers integers integers)))

(define (demo-triples)
  (display-head (pythagorean-triples) 5))



;;; Exercise 3.70

(define (merge-weighted w s1 s2)
  (let ((h1 (stream-car s1))
        (h2 (stream-car s2)))
    (let ((w1 (w h1))
          (w2 (w h2)))
      (cond
       ((<= w1 w2)
        (cons-stream h1
                     (merge-weighted w
                                     (stream-cdr s1)
                                     s2)))
       (else
        (cons-stream h2
                     (merge-weighted w
                                     s1
                                     (stream-cdr s2))))))))


(define (weighted-pairs w s1 s2)
  (let ((h1 (stream-car s1))
        (h2 (stream-car s2)))
    (cons-stream (list h1 h2)
                 (merge-weighted w
                                 (stream-map
                                  (lambda (s) (list s h1))
                                  (stream-cdr s2))
                                 (weighted-pairs w
                                                 (stream-cdr s1)
                                                 (stream-cdr s2))))))

;;A
(define (sum x) (apply + x))
(define (ex-3-71-a) (weighted-pairs sum integers integers))


;;B
(define (nonmultiples-of . L)
  (stream-filter
   (lambda (y) (all (lambda (x) (not (= 0 (remainder y x)))) L))
   integers))

(define (ex-3-71-b)
  (let ((source (nonmultiples-of 2 3 5))
        (weight (lambda (x)
                  (let ((i (car x)) (j (cadr x)))
                    (+ (* 2 i)
                       (* 3 j)
                       (* 5 i j))))))
    (weighted-pairs weight source source)))



;;; Exercise 3.71
(define (duplicated stream)
  (define (find-duplicates last stream)
    (let ((new (stream-car stream)))
      (if (= new last)
          (cons-stream new
                       (skip-duplicates new (stream-cdr stream)))
          (find-duplicates new (stream-cdr stream)))))
  (define (skip-duplicates last stream)
    (let ((new (stream-car stream)))
      (if (= new last)
          (skip-duplicates new (stream-cdr stream))
          (find-duplicates new (stream-cdr stream)))))
  (find-duplicates 0 stream))

(define (sum-cubes x) (apply + (map * x x x)))

(define (ramanujan-numbers)
  (duplicated (stream-map sum-cubes
                          (weighted-pairs
                           sum-cubes integers integers))))

;; 1729
;; 4104
;; 13832
;; 20683
;; 32832
;; 39312
;; 40033
;; 46683
;; 64232
;; 65728 ...


;;; Exercise 3.72

(define (sum-squares x) (apply + (map * x x)))

(define (collect-by-weight weight stream)
  (define (iter current-weight
                examples
                stream)
    (let* ((new-item (stream-car stream))
           (new-weight (weight new-item))
           (rest (stream-cdr stream)))
      (if (= new-weight current-weight)
          (iter current-weight
                (cons new-item
                      examples)
                rest)
          (cons-stream (cons current-weight examples)
                       (iter new-weight (list new-item) rest)))))
  (iter 0 '() stream))

(define (two-squares-three-ways)
  (stream-filter (lambda (x) (> (length x) 3))
                 (collect-by-weight sum-squares
                                    (weighted-pairs sum-squares
                                                    integers
                                                    integers))))

;; racket@chap3.5.rkt> (display-head (two-squares-three-ways) 5)
;; (325 (15 10) (17 6) (18 1))
;; (425 (16 13) (19 8) (20 5))
;; (650 (19 17) (23 11) (25 5))
;; (725 (23 14) (25 10) (26 7))
;; (845 (22 19) (26 13) (29 2))
;; done



;;; Exercise 3.73

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (input v0)
    (add-streams
     (scale-stream input R)
     (integral (scale-stream input (/ 1 C))
               v0
               dt))))

(define (mod a b)
  (- a (* b (floor (/ a b)))))

(define (demo-rc)
  ;; integrate a square wave
  (let*
      ((square-wave
        (stream-map (lambda (x) (if (>= (mod (/ x 10.0) 2) 1) 1.0 -1.0)) integers))
       (RC1 (RC 5 1 0.5))
       (output (RC1 square-wave 0)))
    (display-head output 100)))



;;; Exercise 3.74

;; here's the answer they want...
(define (zero-crossings sense-data)
  (stream-map sign-change-detector
              sense-data (stream-cdr sense-data)))

;; and here's some code to demonstrate it.
(define (sign x)
  (cond ((< x 0) -1)
        ((= x 0) 0)
        ((> x 0) 1)))

(define (sign-change-detector x y)
  (let ((s1 (sign x))
        (s2 (sign y)))
    (cond ((< s1 s2) 1)
          ((= s1 s2) 0)
          ((> s1 s2) -1))))

(define (stream-list L)
  (if (null? L)
      the-empty-stream
      (cons-stream (car L) (stream-list (cdr L)))))

(define (demo-74)
  (let* ((l '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
         (s (stream-list l))
         (z (zero-crossings s)))
    (display-head z (- (length l) 2))))



;;; Exercise 3.75

;; What it's doing is computing a decaying average; it averages the
;; new value with the previous average, not the last value. Fixed as
;; follows:

(define (make-zero-crossings
         input-stream last-value last-average)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream (sign-change-detector avpt last-average)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(define (demo-75)
  (let* ((l '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
         (s (stream-list l))
         (z (make-zero-crossings s 1 1)))
    (display-head z (- (length l) 1))))



;;; Exercise 3.76

(define (smooth stream)
  (define (iter last stream)
    (let ((avg (/ (+ last
                     (stream-car stream))
                  2)))
      (cons-stream avg
                   (iter (stream-car stream) (stream-cdr stream)))))
  (iter (stream-car stream) (stream-cdr stream)))

(define (modular-zero-crossings sense-data)
  (let ((s (smooth sense-data)))
    (stream-map sign-change-detector
                s
                (stream-cdr s))))

(define (demo-76)
  (let* ((l '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
         (s (stream-list l))
         (z (make-zero-crossings s 1 1)))
    (display-head z (- (length l) 1))))



;;; Exercise 3.77

(define (integral3 delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral3 (delay (stream-cdr integrand))
                    (+ (* dt (stream-car integrand))
                       initial-value)
                    dt)))))



;;; Exercise 3.78

;;; Once again, interior defines aren't working right.
(define (solve-2nd a b dt)
  (lambda (dy0 y0)
    (let ( (dy nil)
           (ddy nil)
           (y nil))
      (set! y (integral3 (delay dy) y0 dt))
      (set! dy (integral3 (delay ddy) dy0 dt))
      (set! ddy (add-streams (scale-stream dy a)
                             (scale-stream y b)))
      y)))

(define (demo-78)
  (display-head ((solve-2nd -0.5 -0.5 0.001) 1 1) 1000))



;;; Exercise 3.79

(define (solve-2nd-ord f dt)
  (lambda (dy0 y0)
    (let ((dy nil)
          (ddy nil)
          (y nil))
      (set! y (integral3 (delay dy) y0 dt))
      (set! dy (integral3 (delay ddy) dy0 dt))
      (set! ddy (stream-map f dy y))
      y)))


(define (demo-79)
  (display-head
   ((solve-2nd-ord (lambda (dy y) (+ (- y)
                                (* dy -0.5)))
                   0.1)
    0 1) 100))



;;; Exercise 3.80

(define (RLC r l c dt)
  (lambda (il0 vc0)
    (let ((vc nil) (il nil))
      (set! vc (integral3 (delay (scale-stream il (- (/ 1 C))))
                          vc0 dt))
      (set! il (integral3 (delay (add-streams (scale-stream vc (/ 1 l))
                                              (scale-stream il (/ (- r) l))))
                          il0 dt))
      (cons vc il))))

(define (demo-80)
  (let ((vc-il ((RLC 1 0.2 1 0.1) 0 10)))
    (display-head
     (stream-map list
                 (car vc-il)
                 (cdr vc-il))
     100)))



;;; Exercise 3.81

;; it's not clear how you would use a random number generator
;; generator stream but...

;; the requests are "generate" and "reset"
(define (random-stream stream)
  (define (make-random-stream state stream)
    (if (stream-null? stream) the-empty-stream
        (let* ((request (stream-car stream))
               (new-state
                (cond ((equal? request 'generate)
                       (rand-update state))
                      ((and (pair? request) (equal? (car request) 'reset))
                       (cadr request))
                      (else (error "random-stream: unknown request" request)))))
          (cons-stream new-state
                       (make-random-stream new-state (stream-cdr stream))))))

  (make-random-stream random-init stream))

(define (demo-81)
  (let ((sequence '((reset 12345) generate generate generate
                    (reset 54321) generate generate)))
    (display-stream (random-stream (stream-list sequence)))))



;;; Exercise 3.82

(define (random-range-stream low high)
  (stream-map
   (lambda (x)
     (+ low (/ (* x (- high low)) (expt 2 32))))
   (random-stream (const-stream 'generate))))

(define (in-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

(define (proportion-trials-stream stream)
  (define (iter seen marked stream)
    (let ((new-marked (if (stream-car stream)
                          (+ marked 1)
                          marked))
          (new-seen (+ seen 1)))
      (cons-stream (/ new-marked new-seen)
                   (iter new-seen new-marked (stream-cdr stream)))))
  (iter 0 0 stream))

(define (2d-integral-stream P x1 x2 y1 y2)
  (let ((x (random-range-stream x1 x2))
        (y (random-range-stream y1 y2))
        (scale (* (- x2 x1) (- y2 y1))))
    (let ((inside? (stream-map P x y)))
      (scale-stream (proportion-trials-stream inside?)
                    scale))))

;;; this computes... 2.81????
(define (demo-82)
  (let ((S (2d-integral-stream in-unit-circle? -1.0 1.0 -1.0 1.0)))
    (stream-ref S 100000)))



;;; Miscellaneous non-problem code below.

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random-frac range))))

(define (random-frac range)
  (* (/ (rand) (expt 2 32)) range))

(define random-init 137)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* x c) m) a)))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;;; Again this computes 2.673, not pi.
(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))



(define (integral2 delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

;;; `Solve' as written in the book doesn't work with the SICP package.

;;; fails at runtime (y: undefined: cannot use before initialization
;; (define (solve f y0 dt)
;;   (define y (integral3 (delay dy) y0 dt))
;;   (define dy (stream-map f y))
;;   y)

;; fails at runtime:
;; (define (solve f y0 dt)
;;   (letrec ((y (integral3 (delay dy) y0 dt))
;;            (dy (stream-map f y)))
;;     y))

(define (solve f y0 dt) ;;; this was tricky with scope and order!
  (let ((y nil) (dy nil))
    (set! y (integral3 (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average . args)
  (/ (apply + args) (length args)))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
;; (define pi-stream
;;   (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (stream-filter predicate sequence)
  (cond ((stream-null? sequence) the-empty-stream)
        ((predicate (stream-car sequence))
         (cons-stream (stream-car sequence)
                      (stream-filter predicate (stream-cdr sequence))))
        (else (stream-filter predicate (stream-cdr sequence)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (display-head s n)
  (display-stream (stream-head s n)))

(define (stream-head s n)
  (stream-select (stream-rep true n) s))

(define (stream-rep item n)
  (if (<= n 0)
      the-empty-stream
      (cons-stream item
                   (stream-rep item (- n 1)))))

(define (stream-select mask seq)
  (cond
   ((or (stream-null? mask) (stream-null? seq))
    the-empty-stream)
   ((stream-car mask)
    (cons-stream (stream-car seq)
                 (stream-select (stream-cdr mask)
                              (stream-cdr seq))))
   (else (stream-select (stream-cdr mask) (stream-cdr seq)))))

;;; Streaming

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
       (begin (proc (stream-car s))
              (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (display x) (newline))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

;;; more pastes

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (all pred list)
  ;; apply predicate to each item; return #t if all results are non-#f.
  (if (null? list)
      #t
      (and (pred (car list)) (all pred (cdr list)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square n) (* n n))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;;(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
;;(define fibs (fibgen 0 1))

;;; or even

;;; but but but! this is just trial division!

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
