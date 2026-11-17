(defun fibonacci-digits (n)
  "Calculates the nth fibonacci number using iteration"
  (do ((iter 2 (+ iter 1))
       (n-minus-one 1)
       (n-minus-two 1)
       (result 0))
      ((>= result (expt 10 (- n 1))) iter)
    (setf result (+ n-minus-one n-minus-two))
    (setf n-minus-two n-minus-one)
    (setf n-minus-one result)
    ))



(format t "Fib: ~S~%" (fibonacci-digits 1000))

; 1 1 2 3 5 8 13 
