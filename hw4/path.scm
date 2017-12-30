(begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-file produces a list whose elements are the expressions in the file.

  (define (read-file)
    (let ((expr (read)))
      (if (eof-object? expr)
          '()
          (cons expr (read-file)))))

  ;; Here we go: read in the file that defines the graph

  (define data (with-input-from-file "dist.dat" read-file))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;2D lookup table, taken from notes
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;;Make a table path to store each node from data.dist, and another table to hold
;;the shortest path from any two nodes.
(define path (make-table))
(define short-path (make-table))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Maximum value of any path
(define NMAX 1000000)

;;Procedure to insert dist.data into path table
(define (insert-data nodes)
  (let ((node (car nodes)))
    (cond ((null? (cdr nodes)) "Inserted into table")
          (else (insert! (car node) (car (cdr node)) (car (cdr (cdr node)))
path)
                (insert-data (cdr nodes))))) )

(insert-data data)

;;Procedure to find the node connecting to this node
(define (find-node-to node seq)
    (cond
      ((null? seq) '())
      ((equal? node (caar seq)) (cons (car seq) (find-node-to node (cdr seq))))
      (else (find-node-to node (cdr seq)))))

(define (find-to node) (find-node-to node data))


;;This pulls out every edge from start to another node.
(define (start-node seq)
  (cond
    ((null? seq) '())
    ((equal? 'start (caar seq)) (cons (car seq) (start-node (cdr seq))))
    (else (start-node (cdr seq)))))

;;The procedure takes in a node-list as the start nodes. The val is the minimum
;;path from start to end.
(define (naive-cost node-list val)

  (define NMAX 1000000)

  ;;Helper procedure to find each node starting with symbol node
  (define (find-node node seq)
    (cond
      ((null? seq) '())
      ((equal? node (caar seq)) (cons (car seq) (find-node node (cdr seq))))
      (else (find-node node (cdr seq)))))

  ;;Helper procedure to find the children of a node
  (define (first-child-node node)
    (cond ((null? (find-node (car (cdr node)) data)) '())
        (else (car (find-node (car (cdr node)) data)))))

  ;;Selects the third element in a node
  (define (third-element node)
    (car (cdr (cdr node))))

  ;;Selects the second element in a node
  (define (second-element node)
    (car (cdr node)))

  ;;Helper procedure to return the minimum of two values
  (define (ret-min x y)
    (if (< x y)
        x
        y))
  ;;Helper procedure to calculate the shortest path from start to end
  (define (calculate-cost node val)
    (cond
      ((null? node) NMAX)
      ((equal? 'end (second-element node)) (+ (third-element node) val))
      (else
       (calculate-cost (first-child-node node) (+ val (third-element node))))))
  
  ;;Iterates through each possible path, and returns the minimum from all of the
  ;;paths
  (define (naive node-list val min)
    (cond ((null? node-list) min)
          (else
           (naive (find-node (second-element (car node-list)) data)
                  (+ val (third-element (car node-list)))
                  (naive (cdr node-list) val
                         (ret-min min (calculate-cost (car node-list) val))))
                )))
   (naive node-list val NMAX)
    )

(naive-cost (start-node data) 0))





  
  







