;;Returns the quantity list for the unit being searched.
(define (find val)
  (define (search val source)
    (cond ((null? source) '())
          ((equal? val (car (car source))) (car (cdr (car source))))
          (else (search val (cdr source)))))
  (search val source))

;;Helper procedure to return a number associated with a Quantity
(define (find-qty val)
  (car (find val)))

;;Helper procedure to return a unit-list associated with a Quantity
(define (find-unit-list val)
  (cdr (find val)))

;;Normalize takes a unit-list and returns a quantity of form (a U), with U
;;being in base-units. Normalize consists of many helper procedures and a few
;;complex procedures.
(define (normalize unit-list)

  ;;Simplify reduces the unit-list returned from norm-quantity to 3 or less base
  ;;units.
  (define (simplify unit-list)
    
    ;;If the base-unit is matched, then it adds the exponents
    ;;together. Continues until the list is empty.  IF list is empty and value
    ;;of exponents added together is 0 the procedure returns the empty list;
    ;;This is so we remove empty base units. The return value should be a list
    ;;with one base-unit and a power, or an empty list.
    (define (match-base-unit unit-list unit value)
      (cond  ((and (null? unit-list) (= value 0)) '())            
             ((null? unit-list) (list (list unit value)))         
             ((equal? (select-base unit-list) unit)
              (match-base-unit
               (cdr unit-list) unit
               (+ value (select-power unit-list))))
             (else (match-base-unit (cdr unit-list) unit value))))


    ;;With three passes through we can both simplify it to the form m/kg/sec,
    ;;and put it in a set order. This works because we know that any returned
    ;;unit-list will never contain more than 3 unique units.
    (let ((first-pass (match-base-unit unit-list 'm 0))
          (second-pass (match-base-unit unit-list 'kg 0))
          (third-pass (match-base-unit unit-list 'sec 0)))
      (append first-pass second-pass third-pass)))


  ;;This procedure only gets the unit-list into base-units, and keeps track of
  ;;the value associated with the unit list. A complex calculation is used to
  ;;determine how the "val" is updated. If a unit has a positive exponent, then
  ;;the "val" will be multiplied by the base-unit value raised to the parent
  ;;unit power. If a unit has a negative exponent, then the "val" will be
  ;;multiplied by the base-unit value raised to the parent unit exponent.
  (define (norm-quantity val seq)
   
    (cond ((null? seq) append (list val))
          ;;if we are already in a base-unit, continue to next unit.
          ((or (equal? (select-base seq) 'm)
               (equal? (select-base seq) 'kg)
               (equal? (select-base seq) 'sec))
           (append (list(car seq))
                   (norm-quantity val (cdr seq))))
          ;;If exponent is zero we can skip this unit.
          ((= 0 (select-power seq))
           (norm-quantity val (cdr seq)))
          ;;Positive exponent calculation.
          ((positive? (select-power seq))
           (append (apply-to (select-power seq)
                             (find-unit-list (select-base seq)))
                   (norm-quantity
                    (* val (expt (find-qty (select-base seq))
                                 (select-power seq))) (cdr seq))))
          ;;Negative exponent calculation.
          (else (append
                 (apply-to (select-power seq)
                           (find-unit-list (select-base seq)))
                 (norm-quantity
                  (* val (expt (find-qty (select-base seq))
                               (select-power seq))) (cdr seq))))))

  ;;This is a helper procedure, used to map the parent-unit exponent to the
  ;;base-unit exponent by multiplying them together. It is a bit more
  ;;complicated then a regular map procedure, because we must deal with lists
  ;;within a list.
  (define (apply-to pow seq)
    (define (map-pow seq)
      (define (multiply-pow pow pair)
        (append (list (car pair))
                (list (* pow (car (cdr pair))))))
      (cond ((null? seq) '())        
            (else (append (list (multiply-pow pow (car seq)))
                          (map-pow (cdr seq))))))
    (map-pow seq))

  ;;Helper function to extract a value from the end of a list. norm-quantity
  ;;places the value at the end of the list.
  (define (extract-value seq)
    (cond ((number? (car seq)) (car seq))
          (else (extract-value (cdr seq)))))

  ;;Helper function to eliminate the value from the end of a
  ;;unit-list. norm-quantity places the value at the end of the list, so once we
  ;;hit something that is not a list we can return.
  (define (extract-unit-list seq)
    (cond ((pair? (car seq))
           (append (cons (car seq) (extract-unit-list (cdr seq)))))
          (else '())))

  ;;Helper function to select first unit from the unit-list
  (define (select-base seq)
    (car (car seq)))

  ;;Helper function to select power of the first unit in unit-list
  (define (select-power seq)
    (car (cdr (car seq))))

  ;;Main body of procedure We want to return a quantity in the form (a U)
  (let ((a (extract-value (norm-quantity 1 unit-list)))     
        (U (extract-unit-list (norm-quantity 1 unit-list)))) 

    (append (list a) (simplify U))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Given two ordered unit-lists, we return true if equal, false otherwise.
;;Relatively trivial once we normalize the two unit-lists.
(define (compatible? u-norm v-norm)
  (cond ((and (null? u-norm) (null? v-norm)) #t)
        ((null? u-norm) #f)
        ((null? v-norm) #f)
        ((equal? (car u-norm) (car v-norm))     
         (compatible? (cdr u-norm) (cdr v-norm)))
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Convert. Normalizes the unit-list of from, and to. Then checks if the two
;;unit-lists are compatible.  If compatible it proceeds with conversion,
;;otherwise it returns incompatible units.
(define (convert from to)  

  ;;This procedure takes the unit-list and removes each unit with a power of
  ;;0. Used to test if "to" value is empty.
  (define (remove-zero unit-list)
    (define (select-power seq)
      (car (cdr (car seq))))
    (cond ((null? unit-list) '())
          ((not (equal? (select-power unit-list) 0))
           (append (list (car unit-list)) (remove-zero (cdr unit-list))))
          (else (remove-zero (cdr unit-list)))))

  ;;Variables to hold unit lists and values
  (let   ((u-norm (cdr (normalize (cdr from))))                       
         (v-norm (cdr (normalize to)))                                
         (a-norm (* (car from) (car (normalize (cdr from)))))         
         (b-norm (car (normalize to))))                              
    (cond  ((and (compatible? u-norm v-norm) (null? (remove-zero to))) 
            (append (list 0) to))
           ((compatible? u-norm v-norm)
            (append (list (/ a-norm b-norm)) to))  
           (else (error "Incompatible units cannot convert")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read-file produces a list whose elements are the expressions in the file.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go:  read in the database.

(define source (with-input-from-file "units.dat" read-file))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
