;;(is-list? x). The list? procedure is defined recursively as either the
;;empty-list or a pair whose cdr is a list. We need to evaluate anything that is
;;not a list as false. Then we can determine if x is a pair, and if the final
;;(cdr x) is an empty list. To test this I used arguments that I knew would fail
;;like integers, symbols and pairs not ending in an empty list. I used arguments
;;that I expected to pass like lists, lists within lists, and the empty list.
(define (is-list? x)
  (cond ((null? x) #t)
        ((pair? x) (is-list? (cdr x)))   ;;if not a pair? cannot be a list
        (else #f)))


;;2.18 (my-reverse seq). To reverse a list, we call (list (car seq)) after the
;;recursive procedure instead of before the recursive procedure. I tested with
;;list containing integers, and lists containing a combination of symbols,
;;strings, and integers.
(define (my-reverse seq)
  (cond ((null? seq) '())
        (else (append (my-reverse (cdr seq)) (list (car seq))))))
              

;;2.20 (same-parity w . q). The (same-parity w . q) procedure takes in the
;;arguments and determines if the first argument is an integer?. If it is an
;;integer, it gets funneled into the (add-even q) or (add-odd q) procedure that
;;returns the list with only elements of even or odd parity. I tested this with
;;the first integer being even, odd, and zero. I tested what would happen if the
;;first value, or any value in the list did not contain an integer, and caught
;;that error.
(define (same-parity w . q)
  (define (add-even q)
    (cond ((null? q) '())
          ((not (integer? (car q))) "Error: element in list not an intger")
          ((even? (car q)) (append (list (car q)) (add-even (cdr q))))
          (else (add-even (cdr q)))))
  (define (add-odd q)
    (cond ((null? q) '())
          ((not (integer? (car q))) "Error: element in list not an intger")
          ((odd? (car q)) (append (list (car q)) (add-odd (cdr q))))
          (else (add-odd (cdr q)))))
  (cond ((not (integer? w)) "Error: first element is not an integer")
        ((even? w)  (add-even (append (list w) q)))
        (else (add-odd (append (list w) q)))))


;;2.21 (square-list items). Two different versions of square-list
;;procedure. Tested by using various integer lists, rational numbers, and a list
;;containing zero.
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;;2.23 (my-for-each proc seq). The procedure evaluates the "proc" for each
;;element in the "seq", and returns true after each element is evaluated. Tested
;;with the example from the book.
(define (my-for-each proc seq)
  (cond ((null? seq) (newline) #t)
        (else (proc (car seq)) (my-for-each proc (cdr seq)))))
      

;;2.54 (define (my-equal? a b). This problem is similar to the Pascal triangle
;;problem in chapter one. We need two recursion statements to evaluate equality
;;for lists within lists. (my-equal? (car a) (car b)) recursively checks the
;;first elements are equal, and (my-equal? (cdr a) (cdr b)) recursively checks
;;the remaining elements are equal. I would suspect this has the same flaw as
;;the pascal problem with deeply nested lists taking a very long time to
;;evaluate. I tested this procedure by inputting complicated lists with many
;;nested lists.
(define (my-equal? a b)
  (let ((list-test? (and (pair? a) (pair? b))));;let does not work with (car a)
    (cond (list-test?        
         (if (and (my-equal? (car a) (car b));;Recursively check (car a) (car b)
                 (my-equal? (cdr a) (cdr b)));;Recursively check (cdr a) (cdr b)
             #t
             #f))
        ((eqv? a b) #t)
        (else #f))))
          
    
;;Problem 8 (every? pred seq). The empty list must evaluate to true, because if
;;it did not evaluate to true, then (every? pred (append seq1 seq2)) would not
;;be the same as (and (every? pred seq1) (every? pred seq2)).  This
;;contradiction is key because when you append a list with an empty list it will
;;return the non-empty list. So, take for example, (append '() '(1 2)) will
;;return (1 2). If we took (every? number? '(1 2)) it would return
;;true. However, If the empty list did not return true, then (and (every?
;;number? '()) (every? number? '(1 2))) would return #f, which is impossible if
;;those two expressions are the same. Tested with different predicates, like
;;number?, positive?, integer?.
(define (every? pred seq)
  (cond ((null? seq) #t)
        ((pred (car seq)) (every? pred (cdr seq)))
        (else #f)))

       
;;2.59 A union of two sets is just two lists appended together without
;;duplicates. (remove-val val seq) returns a list without that that
;;value. (remove-duplicates seq) returns a list without any duplicates. The
;;runtime is O(n^2). Tested with sets that contained many duplicates, and sets
;;with different sizes.
(define (unordered-union-set set1 set2)
  (let ((seq (append set1 set2)))
    (define (remove-val val seq)
      (cond ((null? seq) '())
            ((equal? val (car seq)) (append '() (remove-val val (cdr seq))))
            (else (cons (car seq) (remove-val val (cdr seq))))))
    (define (remove-duplicate seq)
      (if (null? seq)
          '()
          (cons (car seq) (remove-duplicate (remove-val (car seq) (cdr seq)))
                )))
    (remove-duplicate seq) ))
  

;;2.62 (ordered-union-set set1 set2) in O(n) time. The runtime can be achieved
;;in linear time because we do not need to check an element in the first set
;;against each element in the second set to create the union. Since the sets are
;;ordered, we only need to handle cases where the (car set1) is equal, greater,
;;or less than (car set2) and build the union set from those conditions. Tested
;;with ordered sets that were different sizes, overlapped, and were empty.
(define (ordered-union-set set1 set2)
  (cond  ((and (null? set1) (null? set2)) '())
         ((null? set1) (append set2 '()))
         ((null? set2) (append set1 '()))
         ((= (car set1) (car set2)) (cons (car set1) (ordered-union-set (cdr
set1) (cdr set2))))
         ((> (car set1) (car set2)) (cons (car set2) (ordered-union-set
set1 (cdr set2))))
         (else (cons (car set1) (ordered-union-set (cdr set1) set2)))))


;;(remove-val val seq). If val equals the first element in seq, we ignore that
;;value. otherwise we recursively cons that element to the returned list. Tested
;;this procedure with "val" that were integers, lists, symbols, and strings.
(define (remove-val val seq)
  (cond ((null? seq) '())
        ((equal? val (car seq)) (append '() (remove-val val (cdr seq))))
        (else (cons (car seq) (remove-val val (cdr seq))))))
