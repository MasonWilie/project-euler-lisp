(defun prime-p (n)
  (cond
    ((< n 2) nil)
    ((< n 4) t)
    ((or (zerop (mod n 2)) (zerop (mod n 3))) nil)
    (t (not
         (loop :for i :from 5 :to (isqrt n) :by 6
           :thereis (or
                      (zerop (mod n i))
                      (zerop (mod n (+ i 2)))))))))

(defun perfect-square-p (n)
  (let ((root (isqrt n)))
    (= (* root root) n)))

(defun goldbachs-conjecture-p (n)
  (loop :for i :from 3 :below n :by 2
    :thereis (and
               (prime-p i)
               (evenp (- n i))
               (perfect-square-p (/ (- n i) 2)))))

(format t "~S~%"
  (loop :for i :from 9 :by 2
    :when (and
            (not (prime-p i))
            (not (goldbachs-conjecture-p i)))
    :return i))

