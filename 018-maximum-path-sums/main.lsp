(require :asdf)

(defun convert-line (line)
  (mapcar #'parse-integer
    (uiop:split-string line :separator " ")))

(defun read-file (filename)
  (with-open-file (file filename :direction :input)
    (loop
      :for line = (read-line file nil)
      :while line
      :collect (convert-line line))))

(defun propagate-nodes (levels)
  (let ((current-level (first levels))
         (next-level (second levels))
         (remaining-levels (cddr levels)))
    (loop :until (null next-level)

      :do (loop :for idx :below (length next-level)
        :do (let* (
                (prev-left-idx (max (- idx 1) 0))
                (prev-right-idx (min idx (- (length current-level) 1)))
                (addition (max
                            (nth prev-left-idx current-level)
                            (nth prev-right-idx current-level))))
              (setf (nth idx next-level) (+ (nth idx next-level) addition))))
      :do (setf current-level next-level)
      :do (setf next-level (car remaining-levels))
      :do (setf remaining-levels (rest remaining-levels)))
    levels))

(format t "~S~%" (reduce #'max
                  (car (last (propagate-nodes (read-file "triangle.txt"))))))

(format t "~S~%" (reduce #'max
                  (car (last (propagate-nodes (read-file "large-triangle.txt"))))))
