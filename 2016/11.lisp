(defpackage "DAY/11"
  (:use "CL"
        "CL-HEAP"))
(in-package "DAY/11")

(defun make-state (building elevator moves)
  (list building elevator moves))



(defun solve (input)
  (let ((current (make-state input 0 0))
        (moves (make-instance 'priority-queue))
        (past-states (fset:set)))
    (loop (cond ((null current)
                 (return :not-found))
                ((winning-p current) (return current))
                (t (map nil (lambda (x)
                              (enqueue x (1+ (moves x))))
                        (next-states current-state past-states))
                   (inludef past-states (building current))
                   (setf current (dequeue moves)))))))
