#lang planet neil/sicp

;;; Exercise 3.1

(define (make-accumulator val)
  (lambda (x) (set! val x) x))

;;; Exercise 3.2

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (case x
        ((how-many-calls?) count)
        (else (set! count (+ 1 count))
              (f x))))))

;;; Exercise 3.3

(define (make-password-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch attempted-password m)
    (cond ((not (equal? attempted-password password))
           (error "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;;; Exercise 3.4

(define (make-alarm-account balance password)
  (let ((count 0))
    (define (call-the-cops) (error "calling the cops!"))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch attempted-password m)
      (if (equal? attempted-password password)
          (begin
            (set! count 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT"
                               m))))
          (begin
            (set! count (+ 1 count))
            (if (>= count 7) (call-the-cops)
                (error "incorrect password")))))
    dispatch))

;; In yet another example of "test your program? what test your
;; program?"  here is a linear congruential random number generator
;; that the book did not ask for but needs.

;; See, "random" should be provided like in MIT scheme, returning floats.

;; So uh, I don't know what's going on here, but both the estimate-pi
;; procedures here estimnate not pi but 2.8295?

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* x c) m) a)))

(define (random-frac range)
  (* (/ (rand) (expt 2 32)) range))

(define random-init 137)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi-stateful trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-pi-stateless trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

;;; Exercise 3.5

;; e.g., (exact->inexact (estimate-integral in-unit-circle? -1 1 -1 1 100000))

(define (in-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random-frac range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (* (- x2 x1) (- y2 y1) (monte-carlo trials test)))

;;; Exercise 3.6

(define (rand-resetting)
  (let ((state random-init))
    (lambda (arg)
      (case (arg)
        ((generate)
         (set! state (rand-update state))
         state)
        ((reset)
         (lambda (newvalue) (set! state newvalue)))))))

;;; Exercise 3.7

(define (make-joint account pwd pwd2)
  (lambda (attempt m)
    (if (equal? attempt pwd2)
        (account pwd m)
        (error "Incorrect password"))))

;; racket@chap3.1.rkt> (define paul (make-password-account 100 'open-sesame))
;; racket@chap3.1.rkt> (define peter (make-joint paul 'open-sesame 'rosebud))
;; racket@chap3.1.rkt> ((peter 'rosebud 'withdraw) 10)
;; 90
;; racket@chap3.1.rkt> ((paul 'open-sesame 'withdraw) 10)
;; 80
;; racket@chap3.1.rkt> ((peter 'open-sesame 'withdraw) 10)
;; Incorrect password
;; racket@chap3.1.rkt> ((paul 'rosebud 'withdraw) 10)
;; Incorrect password

;;; Exercise 3.8

;; Most of the thinking was "what's the simplest such function given
;; that precise requirement?" "One that always returns the last value
;; given to it." sounded simple, but the awkwardness of temp naming
;; complicates it.

;; > (+ (f 0) (f 1))
;; 0
(define f
  (let ((last 0))
    (lambda (x)
      (let ((l last))
        (set! last x)
        l))))
