(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(ql:quickload :fiveam :silent t)

(load "main.lisp")

(defpackage :quadratic-primes-test
  (:use :cl :fiveam)
  (:import-from :cl-user
    #:divisors
    #:prime-p
    #:quadratic-prime-expression
    #:num-consecutive-primes))

(in-package :quadratic-primes-test)

(def-suite quadratic-primes-suite
  :description "Unit tests for quadratic primes")

(in-suite quadratic-primes-suite)

(test divisors-test
  "Test the divisors function"
  (is (equal '() (divisors 1)))
  (is (equal '() (divisors 2)))
  (is (equal '(2 5) (sort (divisors 10) #'<)))
  (is (equal '() (divisors 7)))
  (is (equal '() (divisors 304250263527209)))
  (is (equal '(19 61 67 101 1159 1273 1919 4087 6161 6767 77653 117059 128573 412787) (sort (divisors 7842953) #'<))))

(test prime-p-test
  "Test the prime-p function"
  (is-true (prime-p 2))
  (is-true (prime-p 3))
  (is-false (prime-p 4))
  (is-true (prime-p 7))
  (is-false (prime-p 74981237))
  (is-true (prime-p 304250263527209)))

(test quadratic-prime-expression-test
  "Testing the quadratic prime expression macro"
  (is (= 41 (funcall (quadratic-prime-expression 1 41) 0)))
  (is (= 43 (funcall (quadratic-prime-expression 1 41) 1)))
  (is (= 47 (funcall (quadratic-prime-expression 1 41) 2)))
  (is (= -1 (funcall (quadratic-prime-expression -1 -1) 1))))

(test num-consecutive-primes-test
  "Test num consecutive primes"
  (is (= 40 (num-consecutive-primes 1 41))))

(setf fiveam:*test-dribble* t)
(run! 'quadratic-primes-suite)

