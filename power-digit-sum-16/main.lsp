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

(defun list-power (base exponent)
  "Calculates base^exponent and returns the results as a list (A_0, A_1, ...)
   where the result is A_0 + A_1 * 10 + A_2 * 100 + ..."
  (do ((result '(1)))
      ((zerop exponent) result)
    (list-multiply result base)
    (setf exponent (- exponent 1))))

(defun power-digit-sum (base exponent)
  "Calculates the sum of the digits that result from the calculation base^exponent"
  (reduce #'+ (list-power base exponent)))

(format t "~S~%" (power-digit-sum 2 1000))
