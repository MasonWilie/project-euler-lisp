(defun naive-reduced (num denum)
  (let (
         (num-tens (floor (/ num 10)))
         (num-ones (mod num 10))
         (denum-tens (floor (/ denum 10)))
         (denum-ones (mod denum 10)))
    (cond
      ((and (= num-tens denum-tens) (not (zerop denum-ones))) (/ num-ones denum-ones))
      ((and (= num-tens denum-ones) (not (zerop denum-tens))) (/ num-ones denum-tens))
      ((and (= num-ones denum-tens) (not (zerop denum-ones))) (/ num-tens denum-ones))
      ((and (= num-ones denum-ones) (not (zerop denum-tens))) (/ num-tens denum-tens)))))

(defun digit-canceling-fraction-p (num denum)
  (let* ((reduced (naive-reduced num denum)))
    (and
      (not (null reduced))
      (= reduced (/ num denum)))))

(let (
       (num-product 1)
       (denum-product 1))
  (loop :for num :from 11 :to 99
    :do (loop :for denum :from (+ num 1) :to 99
          :when (and (not (zerop (mod denum 10)))
                  (digit-canceling-fraction-p num denum))
          :do
          (setf num-product (* num-product num))
          (setf denum-product (* denum-product denum))))
  (let* (
          (greatest-common-divisor (gcd num-product denum-product))
          (reduced-num-product (/ num-product greatest-common-divisor))
          (reduced-denum-product (/ denum-product greatest-common-divisor)))
    (format t "~a/~a~%" reduced-num-product reduced-denum-product)))

