;;; Load dependencies
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :fiveam :silent t)

;;; Load the code to test
(load "main.lisp")

;;; Define the test suite
(defpackage :non-abundant-sums-test
  (:use :cl :fiveam)
  (:import-from :cl-user
                #:proper-divisors
                #:abundant-p
                #:get-abundant-numbers
                #:get-sum-of-non-abundant))

(in-package :non-abundant-sums-test)

(def-suite non-abundant-sums-suite
  :description "Tests for Project Euler Non-Abundant Sums problem")

(in-suite non-abundant-sums-suite)

;;; Test proper-divisors function
(test proper-divisors-basic
  "Test proper-divisors function with known values"
  (is (equal '(1 2 3 4 6) (sort (proper-divisors 12) #'<)))
  (is (equal '(1 2 4 7 14) (sort (proper-divisors 28) #'<)))
  (is (equal nil (proper-divisors 1)))
  (is (equal '(1) (proper-divisors 2)))
  (is (equal '(1 2) (sort (proper-divisors 4) #'<)))
  (is (equal '(1 2 4 5 10) (sort (proper-divisors 20) #'<))))

;;; Test abundant-p function
(test abundant-p-non-abundant
  "Test that non-abundant numbers are correctly identified"
  (is (not (abundant-p 1)))
  (is (not (abundant-p 2)))
  (is (not (abundant-p 10)))
  (is (not (abundant-p 11)))
  ;; 28 is perfect (not abundant): divisors 1,2,4,7,14 sum to 28 = 28
  (is (not (abundant-p 28))))

(test abundant-p-abundant
  "Test that abundant numbers are correctly identified"
  ;; 12 is abundant: divisors 1,2,3,4,6 sum to 16 > 12
  (is (abundant-p 12))
  ;; 18 is abundant: divisors 1,2,3,6,9 sum to 21 > 18
  (is (abundant-p 18))
  ;; 20 is abundant: divisors 1,2,4,5,10 sum to 22 > 20
  (is (abundant-p 20))
  ;; 24 is abundant: divisors 1,2,3,4,6,8,12 sum to 36 > 24
  (is (abundant-p 24)))

;;; Test get-abundant-numbers function
(test get-abundant-numbers-hash
  "Test get-abundant-numbers returns correct hash table"
  (let ((abundant-hash (get-abundant-numbers 30)))
    ;; Check that known abundant numbers are present
    (is-true (gethash 12 abundant-hash))
    (is-true (gethash 18 abundant-hash))
    (is-true (gethash 20 abundant-hash))
    (is-true (gethash 24 abundant-hash))
    ;; Check that non-abundant numbers are not present
    (is-false (gethash 1 abundant-hash))
    (is-false (gethash 10 abundant-hash))
    (is-false (gethash 11 abundant-hash))
    (is-false (gethash 28 abundant-hash))))

;;; Test get-sum-of-non-abundant function
(test get-sum-of-non-abundant-23
  "Test sum of non-abundant numbers up to 23"
  ;; For numbers 1-23, all cannot be written as sum of two abundant numbers
  ;; Sum of 1 to 23 = 23 * 24 / 2 = 276
  (is (= 276 (get-sum-of-non-abundant 23))))

(test get-sum-of-non-abundant-30
  "Test sum of non-abundant numbers up to 30"
  ;; For 1-30: from 24-30, only 24 and 30 can be written as sum of two abundant numbers
  ;; 24 = 12 + 12, 30 = 12 + 18
  ;; So sum = 276 (1-23) + 25 + 26 + 27 + 28 + 29 = 276 + 135 = 411
  (is (= 411 (get-sum-of-non-abundant 30))))

;;; Run the tests
(format t "~%========================================~%")
(format t "Running Non-Abundant Sums Tests~%")
(format t "========================================~%~%")

(let ((results (run 'non-abundant-sums-suite)))
  (format t "~%========================================~%")
  (if (fiveam:results-status results)
      (progn
        (format t "All tests passed!~%")
        (format t "========================================~%~%")
        (sb-ext:exit :code 0))
      (progn
        (format t "Some tests failed!~%")
        (format t "========================================~%~%")
        (sb-ext:exit :code 1))))
