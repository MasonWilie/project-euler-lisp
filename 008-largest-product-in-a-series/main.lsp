(defun read-number ()
  "Reads the number from a file names number.txt"
  (with-open-file (stream "number.txt" :direction :input)
   (mapcar 'digit-char-p (coerce (read-line stream nil :eof) 'list))))

(defun largest-adjacent-product (num-list n)
  "Finds the largest product of n adjacent digits in num-list"
  (do* ((idx 0 (+ idx 1))
	(N-Nums (subseq num-list 0 n) (subseq num-list idx (+ idx n)))
	(max-product (reduce #'* n-nums) (max max-product (reduce #'* n-nums))))
       ((eq idx (- (length num-list) n 1)) max-product)))


(format t "~S~%" (largest-adjacent-product (read-number) 13))
