(defpackage "AOC/16"
  (:use "CL"
        "SPLIT-SEQUENCE"
        "STRING-CASE"))
(in-package "AOC/16")

(defun parse-spin (n)
  (list 'spin (parse-integer n)))

(defun parse-exchange (pair)
  (cons 'exchange (mapcar 'parse-integer (split-sequence #\/ pair))))

(defun parse-partner (pair)
  (cons 'partner (mapcar 'intern (mapcar 'string-upcase (split-sequence #\/ pair)))))

(defun parse-instruction (i)
  (let ((payload (subseq i 1)))
    (ecase (aref i 0)
      (#\x (parse-exchange payload))
      (#\p (parse-partner payload))
      (#\s (parse-spin payload)))))

(defparameter +program+
  (apply 'vector
         (loop
           :for code :from (char-code #\a) :to (char-code #\p)
           :collect (intern (string-upcase (string (code-char code)))))))

(defparameter +instructions+
  (with-open-file (in #P"16.input")
    (map 'list 'parse-instruction (split-sequence #\, (read-line in)))))

(defparameter +example-program+ (map 'vector (lambda (x)
                                               (intern (string-upcase (string x)))) "abcde"))
(defparameter +example-instructions+
  '((spin 1)
    (exchange 3 4)
    (partner e b)))

(defun spin (program n)
  (let ((tail (subseq program 0 (- (length program) n)))
        (head (subseq program (- (length program) n))))
    (concatenate 'vector head tail)))
(spin #(0 1 2 3 4) 1)

(defun exchange (program i j)
  (let ((temp (aref program i)))
    (setf (aref program i) (aref program j))
    (setf (aref program j) temp)
    program))

(defun partner (program a b)
  (let ((i (position a program))
        (j (position b program)))
    (exchange program i j)))

(defun solve/1 (program instructions)
  (if (endp instructions)
      program
      (solve/1 (let ((next-program (apply (caar instructions) program (cdar instructions))))
                 next-program)
               (cdr instructions))))

#+(or)
(solve/1 +example-program+ +example-instructions+) ; => #(B A E D C)
;; (solve/1 +program+ +instructions+) ; => #(E H D P I N C A O G K B L M F J)

(defun compute-permutation (start end)
    (loop :for x :across start
          :for i :from 0
          :collect (position x end)))

(defparameter +transform+ (compute-permutation +program+ #(E H D P I N C A O G K B L M F J)))
(defparameter +example-transform+ (compute-permutation +example-program+ #(B A E D C)))

(defun execute-transformation (program transform)
  (let ((next-program (make-array (length program))))
    (loop :for i :from 0
          :for j :in transform
          :do (setf (aref next-program j) (aref program i)))
    next-program))

(defun solve/2 (program instructions)
  (dotimes (i 1000000000)
    (setf program (solve/1 program instructions)))
  program)

#+(or)
(defun solve/2 (program transformation)
  (dotimes (i 4)
    (setf program (execute-transformation program transformation)))
  program)

#+(or)
(solve/2 +example-program+ +example-transform+)
 ; => (1 0 4 3 2)
