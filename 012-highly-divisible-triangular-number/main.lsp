(defun get-divisors (n)
  "Get a list of divisors of n"
  (do ((i 1 (+ i 1))
       (divisors nil))
      ((> i (sqrt n)) divisors)
    (if (zerop (mod n i)) (progn
			    (push i divisors)
			    (if (not (eq (/ n i) i))
				(push (/ n i) divisors))))))

(defun triangular-number-with-n-divisors (n)
  "Find the first triangular number with n divisors"
  (do* ((i 1 (+ i 1))
	(triangular-number i (+ triangular-number i)))
       ((> (length (get-divisors triangular-number)) n)
	triangular-number)))

(format t "~S~%" (triangular-number-with-n-divisors 500))
