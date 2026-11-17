(defun spiral-diagonal-sum (n)
  "Calculate the sum of the diagonals in an n x n
   square created by counting up in a spiral."
  (let ((last-diagonal 1)
         (step 0))
    (+ 1
      (loop :for idx :from 0 :below (* 2 (- n 1))
        :do (if (zerop (mod idx 4))
              (setf step (+ step 2)))
        :do (setf last-diagonal (+ last-diagonal step))
        :sum last-diagonal))))

(format t "~S~%" (spiral-diagonal-sum 1001))
