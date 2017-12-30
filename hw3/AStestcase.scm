;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Convert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (test proc from to expected)
  (display "(convert ")
  (display from)
  (display " ")
  (display to)
  (display ")")
  (display "   ---> Actual: ")
  (display (proc from to))
  (display "  Expected: ")
  (display expected)
  (newline))

(test (lambda (x y) (* x y)) 2 3 6)  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Testing - Odd units
(display (convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1))))  (newline)
(display(convert '(1 (mi 1)(hr -1)) '((furlong 1)(fortnight -1))))      (newline)
(display(convert '(10 (mi 1)(hr -1)) '((furlong 1)(fortnight -1))))(newline)
(display(convert '(10 (mi 1)(hr -2)) '((furlong 1)(fortnight -2))))    (newline)
(display(convert '(10 (mi 1)(hr 1)) '((furlong 1)(fortnight 1))))(newline)

(newline)
;;Testing - Test complex units
(display(convert '(10 (joule 3)(N 4)(hr 2)) '((mi 10)(g 7)(fortnight -12))))(newline)


(newline)
;;Testing - Odd units to base-units
(display(convert '(27.5 (furlong 1)(fortnight -1)) '((m 1)(sec -1))))(newline)
(display(convert '(27.5 (furlong 1)(fortnight -2)) '((m 1)(sec -2))))(newline)
(display(convert  '(27.5 (m 1)(sec -1))  '((furlong 1)(fortnight -1))))(newline)
(display(convert  '(27.5 (m 1)(sec -2)) '((furlong 1)(fortnight -2))))(newline)

(newline)
;;testing - mix of base-odd to base-odd
(display(convert  '(27.5 (m 1)(sec -2)) '((furlong 1)(sec -2))))(newline)
(display(convert  '(27.5 (m 1)(sec -2)) '((m 1)(fortnight -2))))(newline)
(display(convert  '(27.5 (m 1)(kg -2)) '((furlong 1)(kg -2))))(newline)

(newline)
;;Testing - misc
(display(convert  '(27.5 (m 1)(sec -2)) '((m 1)(sec -2))))(newline)
(display(convert '(27.5 (m 0)(sec 0)) '((m 1)(sec 1))))(newline)
(display(convert '(27.5 (m 0)(sec 0)) '((m 0)(sec 0))))      (newline)
(display(convert '(27.5 (m 0)(sec 1))  '((m 0)(hr 1))))(newline)
(display(convert '(27.5 (m 1)(sec 0))  '((m 0)(hr 1))))(newline)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


