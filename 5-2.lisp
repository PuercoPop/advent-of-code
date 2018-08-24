(defpackage "DAY/5-2"
  (:use "CL"))
(in-package "DAY/5-2")

(defparameter +input-file+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/5.input")

(defun condition-1 (string)
  (loop
    :for index :from 0
    :for pair := (subseq string index (+ 2 index))
    :for rest := (subseq string (+ 2 index))
    :until (eql (+ index 2) (length string)) 
    :when (search pair rest)
      :do (return t)))

(defun condition-2 (string)
  (loop :for index :from 0
        :until (eql (+ 2 index) (length string))
        :when (char-equal (aref string index)
                          (aref string (+ 2 index)))
          :do (return t)))

(condition-2 "qjhvhtzxzqqjkmpb") ; => T
(condition-2 "uurcxstgmygtbstg")

(condition-1 "qjhvhtzxzqqjkmpb") ; => T
(defun nice-string-p (line)
  (and (condition-1 line)
       (condition-2 line)))

(with-open-file (in +input-file+)
  (count t
         (loop :for line := (read-line in nil 'eof)
               :until (eq line 'eof)
               :collect (nice-string-p line))))
