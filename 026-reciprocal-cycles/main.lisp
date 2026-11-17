(defun reciprocal-cycle-remainders (numerator denominator &optional (remainders nil))
  (let ((remainder (mod (* 10 numerator) denominator)))
    (cond
      ((zerop remainder) nil)
      ((member remainder remainders)
        (member remainder (reverse remainders)))
      (t (reciprocal-cycle-remainders
           remainder
           denominator
           (cons remainder remainders))))))

(defun max-reciprocal-cycle-remainders (n)
  (do* (
         (i 2 (+ i 1))
         (n-length 0 (length (reciprocal-cycle-remainders 1 i)))
         (max-length 0 (max n-length max-length))
         (max-n 0 (if (= n-length max-length) i max-n)))
    ((= i n) max-n)))

(format t "~S~%" (max-reciprocal-cycle-remainders 1000))
