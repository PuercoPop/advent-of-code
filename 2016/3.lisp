(defpackage "DAY3"
  (:use "CL"
        "SPLIT-SEQUENCE")
  (:shadow "STEP"))
(in-package "DAY3")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/day3-input.txt")

(defun process-line (line)
  (flet ((emptyp (string) (string= "" string)))
      (mapcar 'parse-integer (remove-if #'emptyp (split-sequence #\Space line)))))

(defun possible-triangle-p (side)
  (destructuring-bind (x y z) side
    (and (> (+ x y) z)
         (> (+ x z) y)
         (> (+ z y) x))))

(with-open-file (in +input+)
  (length
   (loop :for line := (read-line in nil 'eof)
         :until (eq 'eof line)
         :for triangle := (process-line line)
         :when (possible-triangle-p triangle)
           :collect triangle)))

(with-open-file (in +input+)
  (let ((counter 0))
    (tagbody
     step
       (setf counter (+ counter
                        (count-if 'possible-triangle-p
                                  (handler-case
                                      (loop
                                        :with (x y z) := ()
                                        :for i :from 0 :upto 2
                                        :for line := (read-line in t)
                                        :for sides := (process-line line)
                                        :do (push (first sides) x)
                                            (push (second sides) y)
                                            (push (third sides) z)
                                        :finally (return (list x y z)))
                                    (end-of-file (c) (declare (ignore c))
                                      (go end))))))
       (go step)
     end)
    counter))
