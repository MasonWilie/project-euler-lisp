(defun list-multiply (digits x)
  "Multiplies the list 'digits' by the value 'x' handling any carry over"
  (do ((carry 0)
       (head digits (rest head)))
      ((and (eq carry 0) (null head)) digits)
    (setf (car head)
	  (+ (* (car head) x) carry))
    (setf carry
	  (floor (/ (car head) 10)))
    (setf (car head)
	  (mod (car head) 10))
    (if (and (null (rest head))
	     (not (zerop carry)))
	(setf (cdr head) (list 0)))))

(defun list-factoral (n)
  "Calculates the factoral n! where n! = 1 * 2 * ... * n"
  (do ((result '(1)))
      ((zerop n) result)
    (list-multiply result n)
    (setf n (- n 1))))

(defun factoral-digit-sum (n)
  "Calculates the sum of the digits that result from the calculation base^exponent"
  (reduce #'+ (list-factoral n)))

(format t "~S~%" (factoral-digit-sum 100))
