(defun read-numbers ()
  "Reads the file 'numbers.txt' which contains 100 50-digit numbers, each on a new line"
  (with-open-file (stream "numbers.txt" :direction :input)
    (loop for line = (read-line stream nil)
	  while line
	  collect (reverse (mapcar 'digit-char-p (coerce line 'list))))))

(defun sum-list-numbers (a b &optional (carry 0))
  "Sums two numbers that are formatted as lists (a_0, a_1, ...) + (b_0, b_1, ...)"
  (if (or (null a) (null b))
      (if (zerop carry) nil (list carry))
      (cons (mod (+ (car a) (car b) carry) 10)
	    (sum-list-numbers (rest a) (rest b)
			      (floor (+ (car a) (car b) carry))))))


(format t "~S~%" (sum-list-numbers '(1 2 3) '(4 5 6)))
