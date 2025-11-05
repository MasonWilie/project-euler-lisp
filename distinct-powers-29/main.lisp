(defun distinct-powers (max-a max-b)
  "Determine the number of distinct values that are produced
   by a^b where 2 <= a <= max-a and 2 <= b <= max-b"
  (length
    (remove-duplicates
      (loop :for a :from 2 :to max-a
        :append (loop :for b :from 2 :to max-b
                  :collect (expt a b))))))

(format t "~S~%" (distinct-powers 100 100))
