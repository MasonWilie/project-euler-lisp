(defun divisors (n)
  "Return the divisors (excluding 1 and n) of n"
  (remove-duplicates
    (loop :for i :from 2 :below (ceiling (sqrt n))
      :when (zerop (mod n i))
      :append (list (/ n i) i))))

(defun prime-p (n)
  "Return if n is prime"
  (cond
    ((< n 2) nil)
    ((= n 2) t)
    ((evenp n) nil)
    (t (loop :for i :from 3 :to (isqrt n) :by 2
         :never (zerop (mod n i))))))

(defmacro quadratic-prime-expression (a b)
  `(lambda (n) (+ (* n n) (* ,a n) ,b)))

(defun num-consecutive-primes (a b)
  "Number of consecutive primes produced by the formula n^2 + an + b"
  (let ((expression (quadratic-prime-expression a b)))
    (loop :for i :from 0
      :while (prime-p (funcall expression i))
      :finally (return i))))

(defun product-of-max-num-consecutive-primes (max-abs-a max-abs-b)
  "Find the product of a and b that produce the maximum number of
   consecutive primes, where |a| < max-abs-a and |b| < max-abs-b"
  (let ((max-num-consecutive-primes 0)
         (max-num-consecutive-primes-a-b-product 0))
    (loop :for a :from (- max-abs-a) :to max-abs-a
      :do (loop :for b :from (- max-abs-b) :to max-abs-b
            :do (let ((num-cp (num-consecutive-primes a b)))
                  (if (> num-cp max-num-consecutive-primes)
                    (progn
                      (setf max-num-consecutive-primes num-cp)
                      (setf max-num-consecutive-primes-a-b-product (* a b)))))))
    max-num-consecutive-primes-a-b-product))

(format t "~S~%" (product-of-max-num-consecutive-primes 1000 1000))

