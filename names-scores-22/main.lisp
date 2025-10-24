(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :split-sequence))

(defun read-names (filename)
  "Read comma-separated names from FILENAME, remove quotes, and return a sorted vector.

  Reads a file containing comma-separated quoted names, strips the quotes,
  and returns a vector of name strings sorted alphabetically."
  (with-open-file (stream filename :direction :input)
    (let ((names (make-array 0 :adjustable t)))
      (loop :for line = (read-line stream nil)
        :while line
        :do (setf names
              (concatenate 'vector names
                (coerce (mapcar
                          #'(lambda (s) (remove #\" s))
                          (split-sequence:split-sequence #\, line))
                  'vector))))
      (sort names #'string<))))

(defun individual-name-score (name)
  "Calculate the alphabetical value of NAME.

  Returns the sum of each letter's position in the alphabet (A=1, B=2, etc.)."
  (loop :for char :across name
    :sum (+ (-
              (char-code char)
              (char-code #\A)) 1)))

(defun total-name-score (names)
  "Calculate the total score for a vector of NAMES.

  For each name, multiplies its alphabetical value by its position (1-indexed)
  in the sorted list and returns the sum of all such products."
  (loop
    :for name :across names
    :for idx :from 1 :to (length names)
    :sum (* idx (individual-name-score name))))


(format t "~S~%" (total-name-score (read-names "names.txt")))
