(eval-when (:compile-toplevel :load-toplevel :execute)
    (ql:quickload :cl-graph)
    (ql:quickload :cl-dot))

(defun make-grid-vertex-graph (n m)
  "Creates a directed graph that represents the corners of a grid"
  (let ((grid-graph (make-instance 'cl-graph:graph-container
                      :default-edge-type :directed
                      :vertex-test #'equal)))

    ;; Add verticies
    (loop for n-idx from 0 to n
      do (loop for m-idx from 0 to m
           do (cl-graph:add-vertex grid-graph (list n-idx m-idx))))

    ;; Add edges
    (loop for n-idx from 0 to n
      do (loop for m-idx from 0 to m
           do (progn
                (if (< n-idx n)
                  (cl-graph:add-edge-between-vertexes
                    grid-graph
                    (list n-idx m-idx)
                    (list (+ n-idx 1) m-idx)))
                (if (< m-idx m)
                  (cl-graph:add-edge-between-vertexes
                    grid-graph
                    (list n-idx m-idx)
                    (list n-idx (+ m-idx 1)))))))
    grid-graph))

(defun find-num-paths (grid-graph begin end)
  "Find the number of paths in a graph between begin and end"
  (let (
         (paths-from-vertex (make-hash-table :test #'eq))
         (begin-v (cl-graph:find-vertex grid-graph begin))
         (end-v (cl-graph:find-vertex grid-graph end)))
    0))

(defun graph-to-dot-file (graph filename)
  "Write a graph to a DOT file"
  (with-open-file (stream filename
                    :direction :output
                    :if-exists :supersede)
    (format stream "digraph G {~%")
    ;; Write vertices
    (cl-graph:iterate-vertexes graph
      (lambda (v)
        (format stream "  \"~{~a~^,~}\";~%" (cl-graph:element v))))
    ;; Write edges
    (cl-graph:iterate-edges graph
      (lambda (e)
        (format stream "  \"~{~a~^,~}\" -> \"~{~a~^,~}\";~%"
          (cl-graph:element (cl-graph:source-vertex e))
          (cl-graph:element (cl-graph:target-vertex e)))))
    (format stream "}~%")))


(defvar *grid-graph* (make-grid-vertex-graph 5 5))

(graph-to-dot-file *grid-graph* "output.dot")
;; (format t "Number of paths: ~S~%" (find-num-paths *grid-graph* '(0 0) '(2 2)))


;; Analytical solution for this problem
;;
;; In an N x M matrix going from the top left corner to the bottom right corner
;; requires you to make N + M moves (right N times, down M times). This can be
;; framed as a combinatorics problem, where you are choosing N + M different moves
;; out of a total set of k moves.

(defun factorial (n)
  "Calculates n!"
  (reduce #'*
    (loop for i from 1 to n collect i)))

(defun lattice-paths-analytical (n)
  "Calculates the number of paths analytically"
  (/
    (factorial (* n 2))
    (* (factorial n) (factorial n))))

(format t "Factorial: ~S~%" (lattice-paths-analytical 20))
