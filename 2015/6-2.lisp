(defpackage "DAY/6-2"
  (:use "CL"
        "ALEXANDRIA"
        "SPLIT-SEQUENCE"
        "STRING-CASE"))
(in-package "DAY/6-2")

(defparameter +input-file+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2015/day6-input.txt")

(defparameter *grid* (make-array '(1000 1000) :initial-element 0))

;; turn off
;; turn on
;; toggle

(defun parse-command (line)
  (list (read-command line)
        (read-first-pair line)
        (read-second-pair line)))

(defun read-command (line)
  (let* ((first-number-at (position-if 'digit-char-p line))
         (command (subseq line 0 (1- first-number-at))))
    (string-case (command)
      ("turn off" #'turn-off)
      ("turn on" #'turn-on)
      ("toggle" #'toggle))))

(defun read-first-pair (line)
  (let* ((first-number-at (position-if 'digit-char-p line))
         (rest-of-line (subseq line first-number-at))
         (end-of-number-at (position #\Space rest-of-line))
         (pair-string (subseq rest-of-line 0 end-of-number-at))
         (pair (mapcar 'parse-integer (split-sequence #\, pair-string))))
    (cons (first pair) (second pair))))

(defun read-second-pair (line)
  (let ((pair (mapcar 'parse-integer (split-sequence #\, (subseq line (1+ (position #\Space line :from-end t)))))))
    (cons (first pair) (second pair))))

(defun compute-pairs (bottom-left-pair top-right-pair)
  (loop 
        :with y := (cdr bottom-left-pair)
        :until (> y (cdr top-right-pair))
        :append (loop
              :for x :from (car bottom-left-pair) :upto (car top-right-pair)
              :collect (cons x y)
              )
        :do (incf y)))

(compute-pairs (cons 499 499) (cons 500 500))

(defun turn-off-1 (grid x y)
  (unless (zerop (aref grid x y))
    (setf (aref grid x y)
          (1- (aref grid x y)))))

(defun turn-on-1 (grid x y)
  (setf (aref grid x y)
        (1+ (aref grid x y))))

(defun toggle-1 (grid x y)
  (setf (aref grid x y)
        (+ 2 (aref grid x y))))

(defmacro defcommand (name stepper)
  (with-gensyms (grid pair-1 pair-2 pair)
    `(defun ,name (,grid ,pair-1 ,pair-2)
       (dolist (,pair (compute-pairs ,pair-1 ,pair-2))
         (funcall ,stepper ,grid (car ,pair) (cdr ,pair))))))
(defcommand turn-off #'turn-off-1)
(defcommand turn-on #'turn-on-1)
(defcommand toggle #'toggle-1)

;; Command :- (instruction pair-1 pair-2)
(defun execute-command (grid command)
  (funcall (car command) grid (cadr command) (caddr command)))

(defun count-lights (grid)
  (loop :for index :from 0 :upto (1- (apply '* (array-dimensions grid)))
        :sum (row-major-aref grid index)))

(defun read-commands ()
  (mapcar 'parse-command
          (remove-if (lambda (line) (string-equal "" line))
                     (split-sequence:split-sequence #\Newline
                                                    (read-file-into-string +input-file+)))))
;; (dolist (command (read-commands))
;;   (execute-command *grid* command))

;; (count-lights *grid* :on)
