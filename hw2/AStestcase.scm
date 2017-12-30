;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase (is-list? x)

;;Testing - Pass
(display "Testing for (is-list? x) - Pass") (newline)
(display (is-list? (list 1 2 3)))           (newline)
(display (is-list? '(1 2 3)))               (newline)
(display (is-list? '(1 (1 2))))             (newline)
(display (is-list? '(1 (1 . 2) 4)))         (newline)
(display (is-list? '()))                    (newline)

(newline)
;Testing - Fail
(display "Testing for (is-list? x) - Fail") (newline)
(display (is-list? 3))                      (newline)
(display (is-list? 3.4))                    (newline)
(display (is-list? 'a))                     (newline)  
(display (is-list? '(3 . 4)))               (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase 2.18 (my-reverse seq)

;;Testing
(display "Testing for (my-reverse seq) - All")                (newline)
(display (my-reverse (list 1 4 9 16 25)) )                    (newline)
(display (my-reverse (list 1 2 3)) )                          (newline)
(display (my-reverse '(a b c)) )                              (newline)
(display (my-reverse '(1 2 5 23 b s "this")) )                (newline)
(display (my-reverse '()) )                                   (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase 2.20 (parity w . q)

;;Testing
(display "Testing for (same0parity w . q) - All")        (newline)
(display (same-parity 2 3 4 5 7 7 7 4 2) )          (newline)
(display (same-parity 2 5 6 7) )                    (newline)
(display (same-parity 1 3 4 5) )                    (newline)
(display (same-parity 0 1 2 3) )                    (newline)
(display (same-parity 0 1 3 5) )                    (newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase 2.21 (square-list items)

;;Testing
(display "Testing for (square-list items) - All")   (newline)
(display (square-list '(1 2 4 6 8))  )              (newline)
(display (square-list '(12 22 -22 44))  )           (newline)
(display (square-list (list 2 3 5 03 20)) )         (newline)
(display (square-list '(3.4 2.2 1.1))  )            (newline)
(display (square-list '(0 0 0)))                    (newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase 2.23 (my-for-each proc seq)

;Testing
(display "Testing for (my-for-each proc seq) - All")              (newline)
(display  (my-for-each (lambda (x) (newline) (display x))
                       (list 223 22 15)))                         (newline)
(display  (my-for-each (lambda (x) (newline) (display x))
                       (list 57 321 88)))                        (newline)
(display  (my-for-each (lambda (x) (newline) (display x))
                       (list 57 321 88)))                         (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase 2.54 (my-equals? a b)

;;Testing
(display "Testing for (my-equals? a b) - All")              (newline)
(display  (my-equal? '(1 2) '(1 2))  )                      (newline)
(display  (my-equal? '(1 (1 2) 2) '(1 (1 2) 2))  )                      (newline)
(display  (my-equal? '(1 (1 (3 4) 2) 2) '(1 (1 (3 4) 2) 2))  )          (newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase (every? pred seq)

;Testing 
(display "Testing for (every? pred seq) - All")              (newline)
(display (every? number? '(1 2 3 4)))                        (newline)
(display (every? positive? '(1 1 3 4)))                      (newline)
(display (every? number? '()) )                              (newline)          



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase 2.59 (unordered-union-set set1 set2)

;Testing
(display "Testing for (unordered-union-set set1 set2) - All")   (newline)
(display (unordered-union-set '(2 3 4 7) '(1 2))  )             (newline)
(display (unordered-union-set '(1 1 1 1) '(3 4 5 6 7)) )        (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase 2.62 (ordered-union-set set1 set2)

;Testing
(display "Testing for (ordered-union-set set1 set2) - All")     (newline)
(display  (ordered-union-set '(1 2 3 4 5) '(-2 -1 0 1))  )      (newline)
(display  (ordered-union-set '(1 2 3 4 5) '(-1 0 1 2 3 7 8)) )  (newline)
(display  (ordered-union-set '(1 2 3 4) '(-1 0 1 2 3))  )       (newline)
(display  (ordered-union-set '(1 2 3) '(-1 0 1 2 3)) )          (newline)
(display  (ordered-union-set '(1 2 3 5 6 7 8) '(-1 0 1 2 3)) )  (newline)
(display  (ordered-union-set '() '()) )                         (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TestCase (remove-val val seq)

;Testing
(display "Testing for (remove-val val seq) - All")              (newline)
(display (remove-val 5 '(1 5 6 5 5 5))  )                       (newline)
(display (remove-val 4 (list 1 4 3))  )                         (newline)
(display (remove-val "this" '(1 3 5 "this" this)))              (newline)
(display (remove-val 'this  '('this 1 2 3 "that")))             (newline)