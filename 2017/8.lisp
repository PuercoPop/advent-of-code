(defpackage "AOC/8"
  (:use "CL" "SPLIT-SEQUENCE"))
(in-package "AOC/8")

(setf (fdefinition '==) (fdefinition '=))
(setf (fdefinition '!=) (fdefinition '/=))

(defun parse-line (line)
  (let ((xs (split-sequence #\Space line)))
    (make-instance 'instruction
                   :register-name (intern (string-upcase (car xs)))
                   :operation (intern (string-upcase (cadr xs)))
                   :op-value (parse-integer (string-upcase (caddr xs)))
                   :predicate (make-instance 'predicate
                                             :register (intern (string-upcase (elt xs 4)))
                                             :operation (intern (string-upcase (elt xs 5)))
                                             :op-value (parse-integer (elt xs 6))))))


(defparameter +instructions+
  (with-open-file (in #P"8.input")
    (loop
      :for line := (read-line in nil nil)
      :while line
      :collect (parse-line line))))


(defclass instruction ()
  ((register-name :initarg :register-name :reader register-name)
   (operation :initarg :operation :reader operation)
   (op-value :initarg :op-value :reader op-value)
   (predicate :initarg :predicate :reader predicate)))

(defclass predicate ()
  ((register :initarg :register :reader register)
   (operation :initarg :operation :reader operation)
   (op-value :initarg :op-value :reader op-value)))

(defparameter +registers+ (make-hash-table))
(defparameter +max-value+ 0)

(defun predicatep (predicate)
  (let ((register-value (gethash (register predicate) +registers+ 0)))
    (funcall (operation predicate) register-value (op-value predicate))))

(defun execute (instruction)
  (let ((next-value (ecase (operation instruction)
                      (inc (+ (gethash (register-name instruction) +registers+ 0)
                              (op-value instruction)))
                      (dec (- (gethash (register-name instruction) +registers+ 0)
                              (op-value instruction))))))
    (setf (gethash (register-name instruction) +registers+)
          next-value)
    (when (> next-value +max-value+)
      (setf +max-value+ next-value))))

(defun step-instruction (instruction)
  (when (predicatep (predicate instruction))
    (execute instruction)))

(defun solve-1 (instructions)
  (loop :for instruction :in instructions
        :do (step-instruction instruction)))


(loop :for value :being :the :hash-values :in +registers+
      :maximizing value)

