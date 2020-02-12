(defpackage "AOC/14"
  (:use "CL"))

(in-package "AOC/14")

(defparameter +input+ #P"14.input")

;; 7 NLPB => 6 NCLZ
;; 1 VMDT, 6 DCFD => 9 SGRXC

(defun strip (string)
  (string-trim '(#\Space) string))

(defun read-compound (desc)
  (let* ((pivot (position #\Space desc))
         (first (subseq desc 0 pivot))
         (second (subseq desc (1+ pivot))))
    (cons (intern second (find-package 'keyword))
          (parse-integer first))))

(defun collect-compounds (lhs)
  (labels ((r (lhs acc)
             (let ((comma (position #\, lhs)))
               (if comma
                   (let ((head (subseq lhs 0 comma))
                         (rest (subseq lhs (+ 2 comma))))
                     (r rest (cons (read-compound head) acc)))
                   (cons (read-compound lhs) acc))
               )))
    (r lhs ())))

(defun parse-line (line)
  (let* ((pivot (position #\= line))
         (lhs (strip (subseq line 0 pivot)))
         (rhs (strip (subseq line (+ 2 pivot)))))
    (cons (read-compound rhs)
          (collect-compounds lhs))))

(defparameter +rules+
  (with-open-file (in +input+)
    (loop :for line := (read-line in nil nil)
          :while line
          :collect (parse-line line))))

(defun sample (bag)
  (let ((i (random (hash-table-count bag))))
    (elt (alexandria:hash-table-keys bag) i)))

(defun react (bag element-name)
  )

;; Start with ((1 . :fuel)) Don't stop until the only elements is ((X . ore))
(defun solve/1 ()
  (let ((bag (make-hash-table)))
    (setf (gethash :fuel bag) 1)
    (loop
      :until (and (= (hash-table-count bag) 1)
                  (gethash :ore bag))
      :for elem := (sample bag)
      :do (let ((new-elements (react bag elem)))
            (remhash elem bag)
            (dolist (elem new-elements)
              (setf (gethash (car elem)
                             bag)
                    (cdr elem))))
      :finally (return bag))))
