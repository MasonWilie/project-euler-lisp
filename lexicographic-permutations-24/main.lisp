(defun factorial (n)
  "Calculates the factorial of n = n(n-1)(n-2)...1"
  (let ((product 1))
    (loop :for i :from 2 :to n
      :do (setf product (* product i)))
    product))

(defun nth-lexicographic-permutation (values n)
  "Find the nth lexigraphical permutation of the list values (0 indexed)"
  (if (zerop (length values)) '()
    (let* ((values-sorted (sort values #'<))
            (possible-sub-permutations
              (factorial (- (length values) 1)))
            (lhs-value (nth (floor (/ n possible-sub-permutations)) values-sorted)))
      (cons lhs-value (nth-lexicographic-permutation
                        (remove lhs-value values-sorted)
                        (mod n possible-sub-permutations))))))

(format t "Perm: ~S~%" (nth-lexicographic-permutation '(0 1 2 3 4 5 6 7 8 9) 999999))
