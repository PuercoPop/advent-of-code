(defpackage "DAY/02"
  (:use "CL"))
(in-package "DAY/02")

(defparameter +example+ #P"2.example")
(defparameter +input+   #P"2.input")

(defparameter +scores-1+
  '(("A X" . 4)
    ("A Y" . 8)
    ("A Z" . 3)
    ("B X" . 1)
    ("B Y" . 5)
    ("B Z" . 9)
    ("C X" . 7)
    ("C Y" . 2)
    ("C Z" . 6)))

(defparameter +scores-2+
  '(("A X" . 3)
    ("A Y" . 4)
    ("A Z" . 8)
    ("B X" . 1)
    ("B Y" . 5)
    ("B Z" . 9)
    ("C X" . 2)
    ("C Y" . 6)
    ("C Z" . 7)))

(defun solve (input score-table)
  (with-open-file (in input)
    (loop :for line := (read-line in nil)
          :while line
          :sum (cdr (assoc line score-table :test #'string-equal)))))
(solve +input+ +scores-1+) ; => 13526 (14 bits, #x34D6)
(solve +input+ +scores-2+) ; => 14204 (14 bits, #x377C)
