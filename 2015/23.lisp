(defpackage "DAY/23"
  (:use "CL"
        "STRING-CASE"))
(in-package "DAY/23")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2015/23.input")

(defparameter +pc+ 0)
(defparameter +registers+ (make-hash-table))
(defvar *code*)

(defun reset-cpu ()
  (setf +pc+ 0
        (gethash 'a +registers+) 0
        (gethash 'b +registers+) 0))

(defun lookup-register (name)
  (gethash name +registers+))

(defun (setf lookup-register) (new name)
  (setf (gethash name +registers+)
        new))

(defun value-or-register (x)
  (if (numberp x)
      x
      (lookup-register x)))

(defun hlf (register) 
  (setf (lookup-register register) (/ (lookup-register register) 2)))

(defun tpl (register) 
  (setf (lookup-register register) (* (lookup-register register) 3)))

(defun inc (register)
  (setf (lookup-register register) (1+ (lookup-register register))))

(defun jmp (offset)
  (setf +pc+ (+ +pc+ offset -1)))

(defun jie (x y)
  (when (evenp (value-or-register x))
    (setf +pc+ (+ +pc+ y -1))))

(defun jio (x y)
  (when (= 1 (value-or-register x))
    (setf +pc+ (+ +pc+ y -1))))

(defun parse-register-name (register-name)
  (intern (string-upcase register-name)
          "DAY/23"))

(defun digit-char-or-sign-p (char)
  (or (digit-char-p char)
      (char= char #\+)
      (char= char #\-)))

(defun parse-value-or-register (string)
  (if (every 'digit-char-or-sign-p string)
      (parse-integer string)
      (parse-register-name string)))

(defun insert-instruction (opcode x &optional (y nil y-provided-p))
  `(,opcode . (,x ,@(when y-provided-p
                      (list y)))))

(defun emit-1-arg-instruction (opcode line)
  (insert-instruction opcode (parse-value-or-register (subseq line 4))))

(defun parse-2-args (line)
  (let* ((middle-space (position #\Space line :from-end t))
         (initial-space (position #\Space (subseq line 0  middle-space) :from-end t)))
    (values (parse-value-or-register (subseq line (1+ initial-space) (1- middle-space)))
            (parse-value-or-register (subseq line (1+ middle-space))))))

(defun emit-2-arg-instruction (opcode line)
  (multiple-value-bind (x y) (parse-2-args line)
    (insert-instruction opcode x y)))

(defun emit-instruction (num-of-args opcode line)
  (ecase num-of-args
    (1 (emit-1-arg-instruction opcode line))
    (2 (emit-2-arg-instruction opcode line))))

(defun parse-line (line)
  (string-case ((subseq line 0 3))
    ("hlf" (emit-instruction 1 'hlf line))
    ("tpl" (emit-instruction 1 'tpl line))
    ("inc" (emit-instruction 1 'inc line))
    ("jmp" (emit-instruction 1 'jmp line))
    ("jie" (emit-instruction 2 'jie line))
    ("jio" (emit-instruction 2 'jio line))))

(defun load-code (in)
  (setf *code*
        (apply 'vector (loop :for line := (read-line in nil)
                             :for program-counter :from 0
                             :while line
                             :collect (parse-line line)))))

(defun execute-instruction ()
  (let ((instruction (aref *code* +pc+)))
    (apply (car instruction) (cdr instruction)))
  (incf +pc+))

(defun run ()
  (loop :while (< +pc+ (length *code*))
        :do (execute-instruction)))

(with-input-from-string (in "inc a
jio a, +2
tpl a
inc a")
  (reset-cpu)
  (load-code in)
  (run)
  (gethash 'b +registers+))

(with-open-file (in +input+)
  (reset-cpu)
  (load-code in)
  (run)
  (gethash 'b +registers+))

(with-open-file (in +input+)
  (reset-cpu)
  (setf (gethash 'a +registers+) 1)
  (load-code in)
  (run)
  (gethash 'b +registers+))
