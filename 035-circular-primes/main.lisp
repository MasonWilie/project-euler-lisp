(defun prime-p (n)
  "Get the divisors (excluding 1 and n) of n"
  (and (not (eql n 1))
    (or (eql n 2)
      (loop :for i :from 2 :to (ceiling (sqrt n))
        :never (zerop (mod n i))))))

(defun num-digits (n)
  "Get the number of digits in n"
  (+ 1 (floor (log n 10))))

(defun rotate (n)
  (+ (floor (/ n 10))
    (* (expt 10 (floor (log n 10)))
      (mod n 10))))

(defun circular-prime-p (n)
  "Predicate for if n is a circular prime (all rotations are prime)"
  (let ((original-digits (num-digits n)))
    (loop :for r :from 0 :below original-digits
      :always (and (prime-p n)
               (= (num-digits n) original-digits))
      :do (setf n (rotate n)))))

(defun circular-primes-below (n)
  "Count the number of circular primes below n"
  (loop :for i :from 1 :below n
    :count (circular-prime-p i)))

(format t "~S~%" (circular-primes-below 1000000))




