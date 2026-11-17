(format t "~S~%"
  (mod
    (loop :for i :from 1 :to 1000
      :sum (expt i i))
    (expt 10 10 )))
