(defun collatz-sequence-length (start)
  "Determines the length of the collatz sequence"
  (do ((seq-length 1 (+ seq-length 1))
       (value start
	      (cond ((evenp value) (floor (/ value 2)))
		    (t (floor (+ (* 3 value) 1))))))
      ((eq value 1) seq-length)))


(defun longest-collatz-sequence-length (max-start)
  (do* ((start 1 (+ start 1))
	(length 1 (collatz-sequence-length start))
	(max-length 1 (max max-length length))
	(number 1 (if (eq max-length length) start number)))
       ((> start max-start) number)
    (format t "~S~%" start)))


(format t "Number: ~S~%" (longest-collatz-sequence-length 1000000))
