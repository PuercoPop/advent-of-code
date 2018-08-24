(defpackage "AOC/5"
  (:use "CL"))
(in-package "AOC/5")

(defparameter +input+ #P"5.input")

(defun read-instructions (path)
  (let ((memory (make-hash-table)))
    (with-open-file (in path)
      (loop
        :for position :from 0
        :for line := (read-line in nil 'eof)
        :until (eq line 'eof)
        :do (setf (gethash position memory)
                  (parse-integer line))))
    memory))

(defparameter +instructions+ (read-instructions +input+))


(defun solve-1 (input)
  (let ((memory (read-instructions input))
        (pc 0))
    (loop
      :for count :from 0
      :for old-pc := pc
      :until (not (gethash pc memory))
      :do (setf pc (+ pc (gethash pc memory)))
          (if-let  )(incf (gethash old-pc memory))
      :finally (return count))))

(defun solve-2 (input)
  (let ((memory (read-instructions input))
        (pc 0))
    (loop
      :for count :from 0
      :for old-pc := pc
      :until (not (gethash pc memory))
      :do (setf pc (+ pc (gethash pc memory)))
          (if  (>= (gethash old-pc memory) 3)
               (decf (gethash old-pc memory))
               (incf (gethash old-pc memory)))
      :finally (return count))))
