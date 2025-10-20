(defun length-of-number-string (n)
  "Returns the length of the string representation of the number without spaces or dashes"
  (+
    (length
      (remove-if #'(lambda (c) (or (eq #\Space c) (eq #\- c)))
        (format nil "~R" n)))
    (if (and
          (> n 100)
          (not (zerop (mod n 100))))
      (length "and") 0)))

(format t "Total characters: ~S~%"
  (reduce #'+
    (loop for n from 1 to 1000
      collect (length-of-number-string n))))
