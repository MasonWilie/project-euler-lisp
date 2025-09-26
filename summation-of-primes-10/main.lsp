
(defun clear-multiples (n numbers)
  (do ((head (nthcdr n numbers) (nthcdr n head)))
      ((null head) numbers)
    (setf (car head) 0)))

(defun first-non-zero (numbers)
  (do ((head numbers (rest head)))
      ((or (null head)
	   (not (zerop (car head))))
       head)))

(defun clear-composites (numbers)
  (do ((head numbers (first-non-zero (rest head)))
       (idx 0 (+ 1 idx)))
      ((null head)
       numbers)
    (if (or (eq (car head) 0)
	    (eq (car head) 1))
	(setf (car head) 0)
	(progn
	  (clear-multiples (car head) head)
	  (if (zerop (mod idx 10000))
	      (format t "Calculated up to ~S~%" (car head)))))))

(defun create-range (n)
  "Creates a range of numbers from [0, n)"
  (loop for i from 0 below n by 1 collect i))

(defun summation-of-primes (n)
  (reduce #'+ (clear-composites (create-range n))))


(format t "~S~%" (summation-of-primes 2000000))
