;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-for-each f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-for-each f (stream-cdr x)))))

(define (display-stream stream)
  (stream-for-each display-line stream))

(define (display-line x)
  (display x) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem 1.  Produces a the stream of m multiplied by decimal representation of
;;strm.  Tested with both finite, and infinite stream. If the stream was finite,
;;then the return stream would eventually print the empty list once all the
;;digits of strm were exhausted.
(define (mult-stream m strm)

  ;;First value of a, and a-list
  (define a (* m (stream-car strm)))
  (define a-list (number->lod (* m (stream-car strm))))

  ;;Helper procedure to find the smallest number with the same number of digits
  ;;as a-list
  (define (pow seq) (expt 10 (- (length seq) 1))) 

  ;;Procedure to calculate the number digit that can be produced. If the stream
  ;;is finite, then after all of strm is exhausted, the stream will print the
  ;;empty list.
  (define (produce-strm a a-list strm)


    ;;Helper procedure to prepend a zero in-front of the a-list if the number of
    ;;elements in the a-list is less than or equal to the previous a-list.
    (define (prepend-zero seq1 seq2)
      (cond
        ((<= (length seq1) (length seq2))
         (prepend-zero (append (list 0) seq1) seq2))
        (else seq1)))


    ;;Conditional block that checks four different scenarios. If the stream is
    ;;null, then it returns the remaining elements in a-list. If the stream is
    ;;null and a-list is null, then it returns the empty list. If the stream is
    ;;not null, and the predicate matches the requirements to produce, then it
    ;;produces an integer. If the stream cannot produce, then it consumes until,
    ;;it can produce.
    (cond
     ;;Return remaining elements of a-list
     ((stream-null? (force (cdr strm)))
      (list (car a-list) 1 (cdr a-list) strm))
     ;;Produce
     ((< (+ m (remainder a (pow a-list))) (pow a-list))
      (list (car a-list) (remainder a (pow a-list)) (cdr a-list) strm))
     ;;Consume
     (else
      (produce-strm
        (+ (* a 10) (* m (stream-car (stream-cdr strm))))
        (prepend-zero
         (number->lod
          (+ (* a 10)(* m (stream-car (stream-cdr strm)))))
         a-list)
        (stream-cdr strm)))))

  ;;A procedure to create a stream. The stream consists of the values produced
  ;;from the procedure "produce-strm". If the strm is finite, then it will
  ;;eventually return the empty list. Otherwise, it will continue to produce
  ;;infinitely.
  (define (stream-mult seq)
    (cons-stream (car seq)
                 (if (null? (car (cdr (cdr seq))))
                      '()
                      (stream-mult
                       (produce-strm (car (cdr seq))
                                     (car (cdr (cdr seq)))
                                     (car (cdr (cdr (cdr seq)))))))))

  (stream-mult (produce-strm a a-list strm))
  )

;;Helper procedure to translate a number into a list containing the integer's of
;;the number.
(define (number->lod int)
  (let ((seq (string->list (number->string int))))
    (map (lambda (x) (- x 48)) (map char->integer seq))))


;;Problem 2.  Tested the stream returned from the procedure pi, and it was
;;accurate for more than 10000 digits of pi. I did not test for more than 10000
;;digits, because it started to slow down considerably after only a few
;;thousand.

;;Constructor procedure to make a 2x2 matrix
(define (make-matrix a b c d) (list a b c d))
;;Add two matrix together, same as mapping two lists with addition
(define (add-matrix a b)  (map + a b))
;;Select first element a in matrix
(define (select-a matrix) (car matrix))
;;Select second element b in matrix
(define (select-b matrix) (car (cdr matrix)))
;;Select third element c in matrix
(define (select-c matrix) (car (cdr (cdr matrix))))
;;select fourth element d in matrix
(define (select-d matrix) (car (cdr (cdr (cdr matrix)))))

;;Multiply two 2x2 matrices together. Using selectors to make it easier to find
;;each element within matrix.
(define (compose a b)
  (list
   (+ (* (select-a a) (select-a b)) (* (select-b a) (select-c b)))
   (+ (* (select-a a) (select-b b)) (* (select-b a) (select-d b)))
   (+ (* (select-c a) (select-a b)) (* (select-d a) (select-c b)))
   (+ (* (select-c a) (select-b b)) (* (select-d a) (select-d b)))))


;;Define first k-matrix as matrix with k value = 1
(define first-element (make-matrix 1 6 0 3))
;;Define n-matrix as matrix with k value = 0
(define n-element (make-matrix 1 4 0 2))

;;Matrix corresponding to linear transformation. An infinite stream with each
;;successive value as the matrix with k=1,2,3...n
(define (stream-elements first-element)
  (cons-stream first-element
               (stream-elements (add-matrix first-element n-element))))

;;Matrix A is the first matrix with k = 1 
(define A first-element)

;;Stream B, Each k-matrix with k=1,2,3...n
(define B (stream-elements first-element))

;;Solve matrix for x. Used to evaluate if the floor of matrix "a applied to 3"
;;is the same as matrix "a applied to 4"
(define (solve-x matrix x)
  (let ((a (+ (* (select-a matrix) x) (select-b matrix)))
        (b (select-d matrix)))
    (quotient a b)
    ))

;;The left shift matrix. Inserting the quotient value for n.
(define (left-shift quotient-value)
  (make-matrix 10 (* -10 quotient-value) 0 1))

;;Get the next value of stream B
(define (next-B B)
  (stream-car (stream-cdr B)))

;;A procedure pi that takes no arguments. The Procedure returns a stream that
;;produces the digits of pi.
(define (pi)

  ;;A procedure that takes a matrix and a stream. It consumes the stream until
  ;;the quotient of the linear transformation of the matrix applied to 3 and 4
  ;;is the same. When it is the same, the procedure returns a list consisting of
  ;;a digit of pi, the matrix needed to find the next digits of pi, and the
  ;;stream needed to find the next digits of pi.
  (define (produce-pi r-matrix stream)
    (let ((quot-a (solve-x r-matrix 3))
          (quot-b (solve-x r-matrix 4)))
      (cond
       ((= quot-a quot-b)
        (list quot-a (compose (left-shift quot-a) r-matrix) stream))
        (else
         (produce-pi (compose r-matrix (next-b stream)) (stream-cdr stream))
   ))))

  ;;A procedure that creates a stream of pi
  (define (stream-pi first-element)
    (cons-stream (car first-element)
                 (stream-pi (produce-pi (car (cdr first-element))
                                        (car (cdr (cdr first-element)))))))

  ;;returned stream consisting of the digits of pi
  (stream-pi (produce-pi A B)))

