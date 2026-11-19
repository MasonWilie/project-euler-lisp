; Very much not optimal but it works

(defun pandigital-p (n)
  (let ((n-str (write-to-string n)))
    (and
      (= (length n-str) (length (remove-duplicates n-str)))
      (= (length n-str) 9)
      (not (find #\0 n-str)))))

(defun pandigital-products ()
  (loop :for i :from 1 :to 9999
    :append (loop
          :for j :from i :to 9999
              :for product = (* i j)
          :when (pandigital-p (parse-integer (concatenate 'string
                                 (format nil "~a" i)
                                 (format nil "~a" j)
                                 (format nil "~a" product))))
          :collect product)))

(format t "Sum: ~a~%" (reduce #'+ (remove-duplicates (pandigital-products))))

