#lang racket/base
(require (planet neil/sicp))
(require (prefix-in dyoo: (planet dyoo/sicp-concurrency:1:2/sicp-concurrency)))
;; (define set-car! set-mcar!)

;; Workaround because dyoo's make-serializer expects
;; serialized functions with no arguments...
(define (make-serializer)
  (let ((s (dyoo:make-serializer)))
    (define (serialize func)
      (define (execute-args . args)
        (define (go-noargs)
          (apply func args))
        ((s go-noargs)))
      execute-args)
    serialize))

(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

;; Well, the package I'm using only offers serializers, so let's first
;; make a mutex out of a serializer.
(define (make-mutex)
  (let ((cell (list #f))
        (protected (make-serializer)))
    
    (define (test-and-set!)
      (if (car cell) #t (begin (set-car! cell #t) #f)))

    (define (clear!)
      (set-car! cell #f))

    (define (acquire)
      (if ((protected test-and-set!)) (acquire)))

    (define (release)
      ((protected clear!)))
    
    (define (the-mutex m)
      (cond ((eq? m 'acquire) (acquire))       ;retry
            ((eq? m 'release) (release))))
    the-mutex))

(define (demo-mutex)
  (let ((M (make-mutex)))
    (parallel-execute
     (lambda () (sleep 0.1) (M 'acquire) (display "world!") (M 'release))
     (lambda () (M 'acquire) (sleep 0.1) (display "hello!") (M 'release)))
    (newline)))


;;; Exercise 3.38
;;
;; a)
;; step
;; 0    1    2    3
;;
;;           90   45
;;      110
;;           55   35
;;
;;
;;           90   45
;; 100  80
;;           40   50
;;
;;
;;           60   40
;;      50
;;           30   40

;; b) One interleaving gives 90. See solution on paper.


;;; Exercise 3.39
;;
;; There are three results if each routine is serialized, because
;; there are three possible orders. The possibilities are 101, 100, and 121.


;;; Exercise 3.40

;; Sketched on paper; the possibilities cover each power of 10 from
;; 100 through 1000000.

;; With the serialized procedures we only get 10,000.



;;; Exercise 3.41

;; There may be a problem if it is possible for operations like "set!"
;; to operate incompletely and display a wrong balance. I would not
;; expect Scheme to do that. Since getting the balance does not read
;; two state variables, it can't be meaningfully
;; interleaved. (However, imagine that you have a procedure that
;; deposited 1,000,000 then withdrew it again. You might want to
;; disallow reading the balance in th middle of such a maneuver.)



;;; Exercise 3.42

;; This has a problem because two concurrent processes can call the
;; same serialized "withdraw" in an interleaved fashion. While
;; "withdraw" would excluce concurrent deposits or vice versa, charing
;; the same serialized procedure would allow two concurrent
;; withdrawals on one account.



;;; Exercise 3.43

;; There are two possible paths of execution, both diagrammed on
;; paper, and both ending on (some permutation of) 1, 2, 3.

;; A further doagram shows a way to get to 20, 20, 20. However, since
;; the withdraw and deposit procedures are serialized, the changes in
;; money are always the same size as was requested. Therefore the
;; flawed "exchange" still conserves the total amount of money.

(define (demo-balance-swap)
  (let ((A (make-account 10))
        (B (make-account 20))
        (C (make-account 30)))
    (parallel-execute
     (lambda () (exchange A B))
     (lambda () (exchange B C)))
    (list (A 'balance) (B 'balance) (C 'balance))))

;;; Exercise 3.44

;; A transfer will have been correctly accomplished if both the
;; withdraw and deposit of the same size go through; it doesn't really
;; matter in which order. The problem with "exchange" is that it does
;; not have this property; "exchange" occurrs correctly if the
;; instantaneous state of both accounts at the end is the reverse of
;; at the beginning. This criterion is defined for data spanning two
;; objects at the "same" time (which means that this procedure needs
;; to synchronize both objects.)


;;; Exercise 3.45

;; This results in "exchange" holding the serialization on an object,
;; then calling "withdraw" which tries to access it through the same
;; serializer, getting stuck.

(define (demo-stuck)

  (define (make-account-and-serializer balance)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) (balance-serializer withdraw))
              ((eq? m 'deposit) (balance-serializer deposit))
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) balance-serializer)
              (#t (error "Unknown request: MAKE-ACCOUNT" m))))
      dispatch))
  
  (let ((A (make-account-and-serializer 100))
        (B (make-account-and-serializer 50)))
    (serialized-exchange A B)))




;;; Exercise 3.46

;; Diagrammed. Both threads read "false" before setting "true".



;;; Exercise 3.47

;; A)
;; We hold a count of locks. In practice, I may want some way of
;; enforcing that no one locks without eventually unlocking, or no one
;; unlocks without locking, but the problem doesnt' seem to ask for
;; that as mutexes have no such structure
(define (make-semaphore n)
  ;; We hold one mutex, and lock the mutex to work with the count.
  (define (acquire! c) (c 'acquire))
  (define (release! c) (c 'release))
  (define (make-flag) (make-mutex))
  (let ((held 0)
        (flag (make-flag)))
    (define (dispatch message)
      (cond ((eq? message 'acquire)
             (acquire! flag)
             (if (< held n)
                 (begin (set! held (+ 1 held))
                        (release! flag))
                 (begin
                   (acquire! flag)
                   (dispatch 'acquire))))
            ((eq? message 'release)
             (acquire! flag)
             (set! held (- 1 held))
             (release! flag))
            (#t (error "Unknown request: MAKE-SEMAPHORE" message))))
    dispatch))

(define (ticky word n)
  (if (<= n 0) nil
      (begin (write (list word n))
             (newline)
             (sleep 0.1)
             (ticky word (- n 1)))))

(define (demo-semaphore)
  ;; this can only interleave two of these at a time.
  (let ((S (make-semaphore 2)))
    (parallel-execute
     (lambda ()
       (S 'acquire) (ticky 'a 5) (S 'release))
     (lambda ()
       (S 'acquire) (ticky 'b 10) (S 'release))
     (lambda ()
       (S 'acquire) (ticky 'c 15) (S 'release)))))


;; B)
;; Using test-and-set is the same as above only using these definitions:

;; (define (acquire! cell)
;;   (if (test-and-set! cell) 'ok (acquire! cell)))
;; (define (release! cell) (set-car! cell #f)
;;   (define (make-flag) (list #f))


;;; Exercise 3.48

;; The proof is by induction over any number of exchange
;; processes. Suppose that N processes exist in a configuration that
;; completes without deadlocking (in the base case, 0 processes
;; complete trivially.) Consider adding another process to the mix, (exchange A B).

;; In the process of evaluating (exchange A B), suppose that account A
;; is busy. Then the process holding it will complete, because it is just the configuration of N which is known good. Then suppose that A has been acquired successfully. Then 
;; you have not. Alternately suppose that you have successfully acquired A. Then
;; any process that holds B must be one that does not ultimately involve
;; A. In that case, it is not prevented from completing.

;; Another way of saying this is that a graph among resources
;; (containing an edge from A->B every time a process locks resource A
;; before resource B.) Imposing an order on the resources means that the
;; edges defines a partial order among the graph nodes. Such a graph
;; has no cycles.

;; This version of make-account introduces an ID.
(define make-account-and-serializer
  (let ((counter 0))
    (lambda (balance)
      (let ((id counter))
        (set! counter (+ counter 1))
        (define (withdraw amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds"))
        (define (deposit amount)
          (set! balance (+ balance amount))
          balance)
        (let ((protected (make-serializer)))
          (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) balance)
                  ((eq? m 'serializer) protected)
                  ((eq? m 'id) id)
                  (#t (error "Unknown request: MAKE-ACCOUNT" m))))
          dispatch)))))

;;; Then serialized-exchange can work like this:

(define (serialized-exchange account1 account2)
  (let ((id1 (account1 'id))
        (id2 (account2 'id)))
    (if (< id2 id1)
        (serialized-exchange account2 account1)
        (let ((serializer1 (account1 'serializer))
              (serializer2 (account2 'serializer)))
          ((serializer1 (serializer2 exchange))
           account1
           account2)))))

(define (demo-no-deadlock)
  (let ((a (make-account-and-serializer 100))
        (b (make-account-and-serializer 150)))
    (parallel-execute
     (lambda () (serialized-exchange a b))
     (lambda () (serialized-exchange b a))
     (lambda () (serialized-exchange a b))
     (lambda () (serialized-exchange b a))
     (lambda () (serialized-exchange a b))
     (lambda () (serialized-exchange b a)))))

;;; Exercise 3.49

;; Imagine two database processes: One which corrects the primary
;; address for every person, and another which corrects the primary
;; resident for every address.
;;
;; Or something like that. A concrete example is slippery to pin down
;; here because it seems like a workaround can always be devised. The
;; real issue is that at some point it becomes impractical to
;; e.g. roll back all changes to objects 100+ because you discovered
;; you also need to touch object 99.... repeating as necessary.

(define (exchange account1 account2) 
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

                                        ;                    serializers


                                        ;                     DEBUGGING/UTIL
(define (show . args)
  ;; write a debugging message.
  (cond ((null? args)
         (display "\n"))
        ((string? (car args))
         (display (car args))
         (display " ")
         (apply show (cdr args)))
        (else
         (write (car args))
         (display " ")
         (apply show (cdr args)))))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (#t (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
