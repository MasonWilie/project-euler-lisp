(defun list-number (n)
  "Turn a number to a list of its digits"
  (loop :until (zerop n)
    :collect (mod n 10)
    :do (setf n (floor (/ n 10)))))

(defun sum-of-digit-fifth-powers (n)
  "Return the sum of the fifth powers of the digits of n"
  (reduce #'+ (mapcar #'(lambda (x) (expt x 5)) (list-number n))))

(defun perfect-sum-of-digit-fifth-powers-p (n)
  "Return whether n = (sum-of-digit-fifth-powers n)"
  (eql n (sum-of-digit-fifth-powers n)))

(defun sum-of-all-perfect-sdfp ()
  "Return the sum of all perfect digit fifth powers"
  (loop :for i :from 2 :below (* 6 (expt 9 5))
    :when (perfect-sum-of-digit-fifth-powers-p i)
    :sum i))

(format t "~S~%" (sum-of-all-perfect-sdfp))

