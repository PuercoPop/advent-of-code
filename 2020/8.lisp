(defpackage "DAY/08"
  (:use "CL"))
(in-package "DAY/08")

(defparameter +input+ #P"8.input")
(defparameter +example+ #P"8.example")

(defun parse-instruction (in)
  (cons (read in)
        (read in)))

(defparameter +code+ (with-open-file (in +example+)
                       (loop :while (peek-chap in)
                             :collect (parse-instruction))))
