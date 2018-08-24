(defpackage "DAY/5"
  (:use "CL"
        "SB-MD5"))

(in-package "DAY/5")

(defparameter +input+ "ugkcyxxp")

(defun make-candidate (input index)
  (md5sum-string (format nil "~A~A" input index)))

(defun hitp (md5)
  "Return true if the md5 is a hit"
  (and (zerop (aref md5 0))
       (zerop (aref md5 1))
       (> 16 (aref md5 2))))

(defun digit (candidate)
  (format nil "~X" (aref candidate 2)))

(defun solve (input)
  (loop
    :with counter := 0
    :for index :from 0
    :for candidate := (make-candidate input index)
    :while (< counter 8)
    :when (hitp candidate)
      :collect (progn (incf counter)
                      (digit candidate)) ))

(solve +input+)
