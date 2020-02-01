(defpackage "DAY/19"
  (:use "CL"))
(in-package "DAY/19")

(defparameter +input+ 3014603)

(defun solve/1 (elf-count)
  (let ((circle (make-array elf-count :initial-element 1)))
    (flet ((find-next (start)
             (loop :for ix :from start
                   :for bounded-ix := (mod ix elf-count)
                   :until (plusp (aref circle bounded-ix))
                   :finally (return bounded-ix))))
      (loop :for current := 0 :then (find-next next)
            :for next := (find-next (1+ current))
            :until (= current next)
            :do (setf (aref circle current) (+ (aref circle current)
                                               (aref circle next))
                      (aref circle next) 0)
            :finally (return (values (1+ current)
                                     (count-if-not #'zerop circle)
                                     ;; circle
                                     ))))))
