(defun factorial (n)
  "Calculate n(n-1)(n-2)...(1)"
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun factorial-sum (numbers)
  "Calculate the sum of the factoral of multiple numbers"
  (reduce #'+ (mapcar #'factorial numbers)))

(defun digits-list (n)
  "Returns a list version of a number, ex. 145 -> (5 4 1)"
  (if (not (zerop n))
      (cons (mod n 10) (digits-list (floor (/ n 10))))))

(defvar *max-possible* (* 8 (factorial 9)))

(defun sum-all-curious-factorials ()
  "Find the sum of all curious factorials"
  (do* ((n 3 (+ n 1))
	(digits '(3) (digits-list n))
	(f-sum 6 (factorial-sum digits))
	(sum 0 (+ sum (if (eq n f-sum) n 0))))
       ((> f-sum *max-possible*) sum)))



(format t "~S~%" (sum-all-curious-factorials))
