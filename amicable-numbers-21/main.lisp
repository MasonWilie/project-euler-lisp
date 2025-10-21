(defun divisors (n)
  "Returns the divisors of n"
  (loop :for i :from 1 :to (sqrt n)
    :when (zerop (mod n i))
    :append (if (or
                  (= (/ n i) i)
                  (= (/ n i) n))
              (list i)
              (list i (/ n i)))))

(defun sum-of-divisors (n)
  "Returns the sum of all divisors of n"
  (reduce #'+ (divisors n)))

(defun get-all-amicable-numbers (max-n)
  "Returns all amicable numbers, where sum(divisors(x)) = y and sum(divisors(y)) = x"
  (remove-duplicates
    (let ((n-to-sum-of-divisors (make-hash-table :test 'eq)))
      (loop :for i :from 1 :below max-n
        :do (setf (gethash i n-to-sum-of-divisors) (sum-of-divisors i)))
      (loop
        :for n :being :the :hash-keys :of n-to-sum-of-divisors
        :for sod :being :the :hash-values :of n-to-sum-of-divisors
        :when (and (not (= n sod))
                (gethash sod n-to-sum-of-divisors)
                (= (gethash sod n-to-sum-of-divisors) n))
        :collect n))))

(defun get-sum-of-all-amicable-numbers (max-n)
  "Returns the sum of all amicable numbers"
  (reduce #'+ (get-all-amicable-numbers max-n)))

(format t "Sum of all amicable numbers: ~S~%" (get-sum-of-all-amicable-numbers 10000))
