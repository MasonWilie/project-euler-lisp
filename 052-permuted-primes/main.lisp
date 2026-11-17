(defun digits-list (n)
  "Get the digits of n in a list"
  (do* (
         (value n (floor (/ value 10)))
         (digit (mod value 10) (mod value 10))
         (digits (list digit) (push digit digits)))
    ((zerop value) (rest digits))))

(defun same-digits-p (a b)
  (equal
    (sort (digits-list a) #'<)
    (sort (digits-list b) #'<)))

(defun smallest-permuted-multiples ()
  (loop :for i :from 1
    :when (and
            (same-digits-p i (* 2 i))
            (same-digits-p (* 2 i) (* 3 i))
            (same-digits-p (* 3 i) (* 4 i))
            (same-digits-p (* 4 i) (* 5 i))
            (same-digits-p (* 5 i) (* 6 i)))
    :do (return i)))

(format t "~S~%" (smallest-permuted-multiples))

