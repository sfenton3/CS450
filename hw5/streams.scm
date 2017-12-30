;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-foreach f (stream-cdr x)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem 1. Tested by using an integers stream starting from 1, and a Fibonacci
;;stream starting from 0. Made sure that displaying the stream from 0 or a
;;negative integer returns the empty-stream, otherwise it displays the first n
;;elements of the stream on a separate line.
(define (display-n stream n)
  (cond ((= n 1) (stream-car stream))
        ((< n 1) the-empty-stream)
        ((= n 2)
         (display (stream-car stream))
         (display-n (stream-cdr stream) (- n 1)))
        (else
         (display (stream-car stream))
         (newline)
         (display-n (stream-cdr stream) (- n 1)))))

;;Problem 2. Exercise 3.50
;;
;;Question 1: The purpose of apply in the last two lines is to evaluate the
;;procedure "stream-map" with the argument "(cons proc (map stream-cdr
;;argstreams))". The purpose of cons is to make a pair out of the procedure and
;;the argstreams, so that "stream-map" can evaluate the arguments in dotted-tail
;;notation. We cannot leave out the "apply" and "cons", because even though the
;;procedure would still return a stream, when you try to evaluate the stream
;;with "stream-cdr" using a procedure such as "display-n", the procedure will
;;become stuck in a read-evaluate loop infinitely.
;;
;;Question 2: If we replace the last two lines with the two lines given, then it
;;will return a stream that displays the same values as the original
;;"stream-map" procedure. This is because apply evaluates the arguments "proc"
;;and "(map stream-cdr argstreams)" all at the same time, and not
;;individually. It is not "(stream-map proc)" then "(stream-map (map stream-cdr
;;argstreams))" as might be expected, but "(stream-map proc (map stream-cdr
;;argstreams))". This is equivalent to the "(stream-map (cons proc (map
;;stream-cdr argstreams)))" from the original "stream-map" procedure.

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;Problem 3. I tested this by displaying a number of values of notdiv-235 and
;;making sure each was not divisible by either 2,3 or 5. As well as verifying
;;that that first 26 values were under 100.
(define notdiv-235 (cons-stream 1
                   (stream-filter !divisible-235? (stream-cdr integers))))

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (!divisible-235? x)
  (cond
    ((OR (= 0 (remainder x 2)) (= 0 (remainder x 3))  (= 0 (remainder x 5))) #f)
    (else #t)))
