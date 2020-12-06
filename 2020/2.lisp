(defpackage "DAY/02"
  (:use "CL"
        "SPLIT-SEQUENCE"))
(in-package "DAY/02")

(defparameter +input+ #P"2.input")

(defun parse-line (line)
  (destructuring-bind (policy password)
      (split-sequence #\: line)
    (destructuring-bind (bounds letter)
        (split-sequence #\Space policy)
      (destructuring-bind (min-bound max-bound)
          (split-sequence #\- bounds)
        (values (char letter 0)
                (parse-integer min-bound)
                (parse-integer max-bound)
                (string-trim '(#\Space) password))))))


(defun password-validp (line)
  (multiple-value-bind
        (letter min max password)
      (parse-line line)
    (<= min
       (count letter password :test #'char=)
       max)))

(defun part-1 ()
  (with-open-file (in +input+)
    (loop :for line := (read-line in nil)
          :while line
          :count (password-validp line))))

(defun password-validp+ (line)
  (multiple-value-bind
        (letter x y password)
      (parse-line line)
      (let ((x? (char= letter (char password (1- x))))
            (y? (char= letter (char password (1- y)))))
        (or (and x? (not y?))
            (and y? (not x?))))))

(defun part-2 ()
  (with-open-file (in +input+)
    (loop :for line := (read-line in nil)
          :while line
          :count (password-validp+ line))))
