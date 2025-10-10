
(defun get-word (s)
  "Get the next word in a string along with the rest of the string as a dotted pair"
  (let ((space-pos (position #\space s)))
    (if space-pos
	(cons (subseq s 0 space-pos) (subseq s (+ space-pos 1)))
	(cons s nil))))

(defun split-string (s)
  "Split a space delimited string"
  (let ((split (get-word s)))
    (if (null s)
	(car s)
	(cons (car split) (split-string (cdr split))))))

(defun read-grid (file-name)
  "Reads a grid of integers in as a vector of vectors"
  (coerce
   (with-open-file (stream file-name :direction :input)
     (loop for line = (read-line stream nil)
	   while line
	   collect (coerce
		    (mapcar #'parse-integer
			    (split-string line))
		    'vector)))
   'vector))

(defun add-horizontal (n x y)
  "Left to right"
  (values (+ x n) y))

(defun add-vertical (n x y)
  "Up and down"
  (values x (+ y n)))

(defun add-diagonal-right (n x y)
  "Down to the right"
  (values (+ x n) (+ y n)))

(defun add-diagonal-left (n x y)
  "Down to the left"
  (values (- x n) (+ y n)))

(defun direction-product (n x y grid op)
  "Find the product of numbers based on the calculation 'op' which specifies the direction"
  (if (and (< (+ x n) (length (aref grid 0)))
	   (< (+ y n) (length grid)))
      (reduce #'*
	      (loop for idx from 0 below n
		    collect (multiple-value-bind
				  (new-x new-y) (funcall op idx x y)
			      (aref (aref grid new-x) new-y))))
      0))

(defun largest-product-in-a-direction (grid op n max-x max-y &optional (min-x 0))
  "Find the largest product in a direction (specified by op) up to bounds"
  (let ((max-product 0))
    (loop for idx-y from 0 below max-y
	  do (loop for idx-x from min-x below max-x
		   do (setf max-product (max
					 max-product
					 (direction-product n idx-x idx-y grid op)))))
    max-product))

(defun largest-product-in-a-grid (grid n)
  "Find the largest product in a grid in the same direction (horizontal, vertical, or diagonal)"
  (let* ((max-x (length (aref grid 0)))
	 (max-y (length grid))
	 (max-x-bounded (- max-x n))
	 (max-y-bounded (- max-y n)))
    (max
     (largest-product-in-a-direction grid #'add-horizontal n max-x-bounded max-y)
     (largest-product-in-a-direction grid #'add-vertical n max-x max-y-bounded)
     (largest-product-in-a-direction grid #'add-diagonal-right n max-x-bounded max-y-bounded)
     (largest-product-in-a-direction grid #'add-diagonal-left n max-x max-y-bounded n))))

(format t "Largest product: ~S~%" (largest-product-in-a-grid *grid* 4))


