(defparameter *mem* (make-hash-table :test 'equal))

(defun coin-sums (s &optional (coins '(200 100 50 20 10 5 2 1)))
  (cond
    ((zerop s) 1)
    ((or (< s 0) (null coins)) 0)
    (t
      (let ((key (cons s coins)))
        (multiple-value-bind (val exists-p) (gethash key *mem*)
          (if exists-p
            val
            (let ((result (+
                            (coin-sums (- s (first coins)) coins)
                            (coin-sums s (rest coins)))))
              (setf (gethash key *mem*) result)
              result)))))))

(format t "Sum: ~S~%" (coin-sums 200))
