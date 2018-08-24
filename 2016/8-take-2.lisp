(defpackage "DAY/8"
  (:use "CL"
        "ALEXANDRIA"))
(in-package "DAY/8")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/8.input")

(defvar *grid*)
;; make-array (height width)
(defparameter *grid* (make-array '(2 3) :initial-element #\.))

(defun index-pairs (displacement max-index)
  (loop :for i :from 0 :upto (1- max-index)
        :collect (cons i (mod (+ i displacement) max-index))))

(defun build-psetf (row-number pairs code)
  (cond ((null pairs) (cons 'psetf code))
        (t (build-psetf row-number
                        (cdr pairs)
                        (cons `(aref *grid* ,row-number ,(caar pairs))
                              (cons `(aref *grid* ,row-number ,(cdar pairs))
                                    code))))))

(defmacro define-grid (x y)
  (with-gensyms (pairs)
    `(progn
       (setf *grid* (make-array '(,y ,x) :initial-element 0))
       (defun rotate-row (row-number times)
         '(let ((,pairs (index-pairs times (array-dimension *grid* 1))))
            ,(build-psetf row-number pairs nil)))
       *grid*)))


(defmacro build-psetf (pairs row-number)
  `(let ((pairs ,pairs))
     (psetf ,@(loop :for pair :in pairs
                    :collect `((aref *grid* ,row-number ,(car pair))
                               (aref *grid* ,row-number ,(cdr pair)))))))



(build-psetf (index-pairs 4 7) 0 nil)

(defun rotate-row (row-number times)
  (macrolet ((build-psetf (pairs row-number)
               `(psetf ,@(loop :for pair :in pairs
                               :collect `((aref *grid* ,row-number ,(car pair))
                                          (aref *grid* ,row-number ,(cdr pair)))))))
    (let ((index-pairs (index-pairs times (array-dimension *grid* 1))))
      (build-psetf index-pairs row-number))))

;; (aref *grid* row column)
(index-pairs 4 7)
;; En un 7x3
;; (rotate-row 0 4)
;; =>
;;                    row (car pair)             row (cdr pair)
;; (psetf (aref *grid* 0      0)     (aref *grid* 0      4)
;;        (aref *grid* 0 1) (aref *grid* 0 5)
;;        (aref *grid* 0 2) (aref *grid* 0 6)
;;        (aref *grid* 0 3) (aref *grid* 0 0)
;;        (aref *grid* 0 4) (aref *grid* 0 1)
;;        (aref *grid* 0 5) (aref *grid* 0 2)
;;        (aref *grid* 0 6) (aref *grid* 0 3))

(loop :for pair :in (index-pairs 2 3) :collect `((aref x ,(car pair))  (aref x ,(cdr pair))))
(defun rotate-row (row-number times)
  (let ((index-pairs n x))
    (psetf)))



(defun print-grid (grid)
  (loop :for row :in grid
        :do (print-row row)))

(define-grid 7 3)

(rect 3 2)
(rotate-column 1 1)
(rotate-row 0 4)
(rotate-column 1 1)
;; Luego contar bits
(count-pixels +grid+)


(defun parse-rect-parameters (line)
  (let ((first-space (position #\Space line))
        (x (position #\x line)))
    (mapcar 'parse-integer (list (subseq line first-space x)
                                 (subseq line (1+ x))))))

(parse-rect-parameters "rect 19x1")

(defun parse-rotate-parameters (line)
  (let ((= (position #\= line))
        (b (position #\b line))
        (last-space (position #\Space line :from-end t)))
    (mapcar 'parse-integer (list (subseq line (1+ =) (1- b))
                                 (subseq line last-space)))))

(parse-rotate-parameters "rotate column x=20 by 3")
(defun do-command (line)
  (cond ((search "rect" line) (apply 'rect (parse-rect-parameters line)))
        ((search "rotate column" line) (apply 'rotate-column (parse-rotate-parameters line)))
        ((search "rotate row" line) (apply 'rotate-row (parse-rotate-parameters line)))
        (t (error "Command not found."))))
(parse-command "rect 19x1")
(parse-command "rotate column x=20 by 3")
(parse-command "rotate row y=4 by 15")

(define-grid 50 6)
(with-input-from-file (in +input+)
  (loop :for line := (read-line in nil 'eof)
        :until (eq line 'eof)
        :do (do-command line)))
(count-pixels +grid+)
(print-grid +grid+)
