(defpackage "AOC/1"
  (:use "CL"))
(in-package "AOC/1")

(defparameter +input+ #P"1.input")

(defun solve/1 ()
  (with-open-file (in +input+)
    (loop
      :for num := (read in nil)
      :while num
      :sum num)))

(defparameter +deltas+ 
  (let ((deltas (with-open-file (in +input+)
                  (loop
                    :for num := (read in nil)
                    :while num
                    :collect num))))
    (setf (cdr (last deltas))
          deltas)
    deltas))

(defun solve/2 ()
  (loop
    :With seen := (make-hash-table)
    :with freq := 0
    :for num :in +deltas+
    :while (not (gethash freq seen))
    :do
       (setf (gethash freq seen) freq
             freq (+ freq num))
    :finally (return (values freq num seen))))
