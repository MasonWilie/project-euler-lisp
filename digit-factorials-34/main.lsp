(defun factorial (n)
  "Calculate n(n-1)(n-2)...(1)"
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun factoral-sum (numbers)
  "Calculate the sum of the factoral of multiple numbers"
  (reduce #'+ (mapcar #'factorial numbers)))

(defun digits-list (n)
  "Returns a list version of a number, ex. 145 -> (5 4 1)"
  (if (not (zerop n))
      (cons (mod n 10) (digits-list (floor (/ n 10))))))

(defun curious-factorial-p (n)
  "Checks if the sum of the factoral of the digits is equal to the number"
  (eq n (factoral-sum (digits-list n))))


(format t "~S~%" (curious-factorial-p 14))
