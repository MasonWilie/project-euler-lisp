(defun proper-divisors (n)
  "Get the proper divisors (divisors including 1) of n"
  (remove n (remove-duplicates
     (loop :for i :from 1 :to (ceiling (sqrt n))
       :when (zerop (mod n i))
       :append (list i (/ n i))))))

(defun abundant-p (n)
  "Is n an abundant sum, i.e. n is equal to
   the sum of the propor divisors of n"
  (< n (reduce #'+ (proper-divisors n) :initial-value 0)))

(defun get-abundant-numbers (max-n)
   "Get all the abundant sum below max-n"
   (let ((abundant-numbers (make-hash-table :test 'eql)))
     (loop :for i :from 1 :to max-n
       :when (abundant-p i)
       :do (setf (gethash i abundant-numbers) t))
     abundant-numbers))

(defun get-sum-of-non-abundant (max-n)
  "Get the sum of all numbers that cannot be represented as the sum of two abundant numbers"
  (let ((abundant-numbers (get-abundant-numbers max-n)))
    (loop :for i :from 1 :to max-n
      :when (loop :for j :from 1 :below i
        :never (and
                 (gethash j abundant-numbers)
                 (gethash (- i j) abundant-numbers)))
      :sum i)))

(format t "~S~%" (get-sum-of-non-abundant 28123))

