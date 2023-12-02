(defpackage "DAY/1"
  (:use "CL"))
(in-package "DAY/1")

(defparameter +example+ #P"1.example")
(defparameter +input+ #P"1.input")

(defun calibration (line)
  (let ((left (find-if #'digit-char-p line))
        (right (find-if #'digit-char-p line :from-end t)))
    (parse-integer (coerce (list left right) 'string))))

(defun part/1 (input)
  (with-open-file (in input)
    (loop :for line := (read-line in nil)
          :while line
          :sum (calibration line))))

(defparameter +digits+
  '(("one"   . #\1)
    ("two"   . #\2)
    ("three" . #\3)
    ("four"  . #\4)
    ("five"  . #\5)
    ("six"   . #\6)
    ("seven" . #\7)
    ("eight" . #\8)
    ("nine"  . #\9)))

(defun starts-with-p (prefix string)
  (when (<= (length prefix ) (length string))
    (string-equal prefix
                (subseq string 0 (length prefix)))))

(defun spelled-digit-p (line)
  (loop :for (word . char) :in +digits+
        :when (starts-with-p word line)
          :return char))

(defun process-line (line)
  (let (left right)
    (loop :for ix :upto (1- (length line))
          :for sub := (subseq line ix)
          :for first := (char sub 0)
          :for looking-at := (or (and (digit-char-p first)
                                      (digit-char (digit-char-p first)))
                                 (spelled-digit-p sub))
          :when looking-at
            :do (unless left
                  (setf left looking-at))
                (setf right looking-at))
    (parse-integer (coerce (list left right) 'string))))

(defun part/2 (input)
  (with-open-file (in input)
    (loop :for line := (read-line in nil)
          :while line
          :sum (process-line line))))
