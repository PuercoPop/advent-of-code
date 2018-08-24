(defpackage "DAY/6"
  (:use "CL"))
(in-package "DAY/6")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/day6-input.txt")

(defparameter *code-matrix* (make-array '(8 624)))

;; 0 entry por que esta mal el tamaño.
(defun build-frequency-table (array column)
  (let ((table (make-hash-table :test 'equalp)))
    (loop :for row :from 0 :upto (1- (array-dimension array 1))
          :for char := (aref array column row)
          :do
             (setf (gethash char table) (1+ (gethash char table 0))))
    table))
(macrolet ((define-character-comparison  (comparison initial-element)
             ;; Falta
             `(defun (frequency-table)
                  (let ((count initial-element)
                        (result))
                    (maphash (lambda (k v)
                               (when (funcall ,comparison v count)
                                 (setf result (cons k v)
                                       count v)))
                             frequency-table)
                    result)))))

(defun most-frequent-character (frequency-table)
  (let ((max-count most-negative-fixnum)
        (result))
    (maphash (lambda (k v)
               (when (> v max-count)
                 (setf result (cons k v)
                       max-count v)))
             frequency-table)
    result))

(defun least-frequent-character (frequency-table)
  (let ((min-count most-positive-fixnum)
        (result))
    (maphash (lambda (k v)
               (when (< v min-count)
                 (setf result (cons k v)
                       min-count v)))
             frequency-table)
    result))

(with-open-file (in +input+)
  (loop :for line := (read-line in nil 'eof)
        :for y-index :from 0
        :until (eq line 'eof)
        :do
           (loop :for char-code :across line
                 :for x-index :from 0
                 :do (setf (aref *code-matrix* x-index y-index)
                           char-code))))

(loop :for index :from 0 :upto (1- (array-dimension *code-matrix* 0))
      :collect (car (most-frequent-character (build-frequency-table *code-matrix* index))))
