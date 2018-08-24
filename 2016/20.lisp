(defpackage "DAY/20"
  (:use "CL"
        "SPLIT-SEQUENCE"))
(in-package "DAY/20")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/20.input")

(defvar *pairs*)
(with-open-file (in +input+)
  (setf *pairs* (sort (mapcar 'parse-integer (loop :for line := (read-line in nil)
                                                   :while line
                                                   :collect (car (split-sequence #\- line))))
                      '<)))

(loop :With prev-high := 0
      :for (low high) :in *pairs*
      :until (> low prev-high)
      :do (setf prev-high high)
      :finally (return (values  prev-high low high)))

(with-open-file (in +input+)
  (sort (loop :for line := (read-line in nil)
              :while line
              :collect (split-sequence #\- line))
        '< :key (lambda (x) (parse-integer (car x)))))
