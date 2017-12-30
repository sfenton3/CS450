;;Version 1 - Rewritten the dispatch procedure as a lambda. Rewritten the body
;;of each define as a lambda expression. Tested with the examples used in the
;;book.
(define make-account-lambda
  (lambda (balance)
    (define deposit
      (lambda (amount)
        (set! balance (+ balance amount))
         balance))
    (define withdraw
      (lambda (amount)
        (if (>= balance amount)
             (begin (set! balance (- balance amount))
                    balance)
             "Insufficient funds")))
    (lambda (m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else
             (error "Unknown request -- MAKE-ACCOUNT " m))))))


;;Version 2 - Replaced each define statement with a lambda. Removed the dispatch
;;lambda, and replaced it with a "cond" that checks if the symbol is 'withdraw,
;;'deposit or unknown. Tested with same expressions as in Version 1.
(define make-account-inline
  (lambda (balance)
    (lambda (m)
      (cond ((eq? m 'withdraw)
             (lambda (amount)
               (if (>= balance amount)
                (begin (set! balance (- balance amount)) balance)
                "Insufficient funds")))
            ((eq? m 'deposit)
             (lambda (amount)
               (set! balance (+ balance amount))
                balance))
            (else (error "Unknown request -- MAKE-ACCOUNT " m))))))

;;Version 3 - Factor out a lambda.  I factored out the lambda by moving the two
;;lambdas in the "cond", to one lambda outside of the "cond". Tested with same
;;expressions as Version 1.
(define make-account-inline-factored
  (lambda (balance)
    (lambda (m)
      (lambda (amount)
         (cond ((eq? m 'withdraw)
                 (if (>= balance amount)
                 (begin (set! balance (- balance amount)) balance)
                 "Insufficient funds"))
              ((eq? m 'deposit)
                 (set! balance (+ balance amount)) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT " m))
              )))))


;;Problem 3: Exercise 3.2. I was able to keep count of how many times the
;;procedure f was called by creating a local variable "count", and using set! to
;;increment that variable. Tested by running make-monitored with a few single
;;argument procedures, and tested with 'how-many-calls? and 'reset-count worked.
(define make-monitored
  (let ((count 0))
  (lambda (f)
    (lambda (mf)
      (cond ((eq? mf 'how-many-calls?) count)
              ((eq? mf 'reset-count) (set! count 0)) 
              (else (set! count (+ count 1)) (f mf)))))))



;;Problem 4: Exercise 3.3. I used the solution to the first problem "version
;;two" as a black-box, and added an argument to the first and second lambda,
;;that was used to test for a password. When the user first defines a
;;make-pw-account the procedure requires a balance and password. The next time
;;the user calls the procedure, it requires two symbols that represent the
;;dispatch message, and password-attempt. If the password attempt is successful,
;;then the procedure evaluates the amount. I Tested that it worked when correct
;;password was used, and error was evaluated if wrong password was used.
(define make-pw-account
  (lambda (balance pass)
    (lambda (attempt m)        
      (cond
        ((and (eq? m 'withdraw) (eq? pass attempt))
         (lambda (amount)
           (if (>= balance amount)
               (begin (set! balance (- balance amount)) balance)
               "Insufficient funds")))
        ((and (eq? m 'deposit) (eq? pass attempt))
         (lambda (amount)
           (set! balance (+ balance amount))
           balance))
        ((not (eq? pass attempt)) (error "Incorrect Password"))
        (else (error "Unknown request -- MAKE-ACCOUNT " m))
      ))))
