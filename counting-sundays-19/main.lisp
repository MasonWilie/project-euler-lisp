(eval-when
  (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :local-time :silent t))

(defun count-sundays (start-str stop-str)
  "Count the sundays between the start timestamp and the stop timestamp"
  (let (
         (start (local-time:parse-timestring start-str))
         (stop (local-time:parse-timestring stop-str)))
    (loop :for ts = start :then (local-time:timestamp+ ts 1 :day)
      :while (local-time:timestamp<= ts stop)
      :count (and
               (zerop (local-time:timestamp-day-of-week ts))
               (= (local-time:timestamp-day ts) 1)))))

(format t "Sunday Count: ~S~%" (count-sundays "1901-01-01" "2000-12-31"))
