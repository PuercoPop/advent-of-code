(defpackage "DAY/12"
  (:use "CL"
        "STRING-CASE"))
(in-package "DAY/12")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/12.input")

(defparameter +pc+ 0)
(defparameter +registers+ (make-hash-table))
(defvar *code* nil)

(defun reset-cpu ()
  (setf +pc+ 0
        (gethash 'a +registers+) 0
        (gethash 'b +registers+) 0
        (gethash 'c +registers+) 0
        (gethash 'd +registers+) 0))

(defun lookup-register (name)
  (gethash name +registers+))

(defun (setf lookup-register) (new name)
  (setf (gethash name +registers+)
        new))

(defun value-or-register (x)
  (if (numberp x)
      x
      (lookup-register x)))

(defun cpy (x y) 
  (setf (lookup-register y)
        (value-or-register x)))

(defun inc (register) 
  (setf (lookup-register register) (1+ (lookup-register register))))

(defun dec (register)
  (setf (lookup-register register) (1- (lookup-register register))))

(defun jnz (x y)
  (unless (zerop (value-or-register x))
    (setf +pc+ (+ +pc+ y -1))))

(defun read-until-eof (in)
  (loop :for object := (read in nil)
        :while object
        :collect object))

(defun load-code (in)
  (setf *code*
        (apply 'vector (loop :for line := (read-line in nil)
                             :for program-counter :from 0
                             :while line
                             :collect (read-until-eof (make-string-input-stream line))))))

(defun execute-instruction ()
  (let ((instruction (aref *code* +pc+)))
    (apply (car instruction) (cdr instruction)))
  (incf +pc+))

(defun run ()
  (loop :while (< +pc+ (length *code*))
        :do (execute-instruction)))

;; Test
(with-input-from-string (in "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a")
  (reset-cpu)
  (load-code in)
  (run))
;; 1
(with-open-file (in +input+)
  (reset-cpu)
  (load-code in)
  (run)
  (format t "The regiser A holds the value ~A.~%" (gethash 'a +registers+)))
;; 2
(with-open-file (in +input+)
  (reset-cpu)
  (setf (gethash 'c +registers+) 1)
  (load-code in)
  (run)
  (format t "The regiser A holds the value ~A.~%" (gethash 'a +registers+)))
