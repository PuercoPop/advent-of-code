(defpackage "DAY/18"
  (:use "CL"
        "TRIVIA"))
(in-package "DAY/18")

(defparameter +input+ "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^..")
(defparameter +test+ "..^^.")

(defun trap? (&rest prev-tiles)
  ;; left center right
  (match prev-tiles
    ((list :trap :trap :safe) :trap)
    ((list :safe :trap :trap) :trap)
    ((list :trap :safe :safe) :trap)
    ((list :safe :safe :trap) :trap)
    (otherwise :safe)))

(trap? :safe :safe :safe) ; => :SAFE
(trap? :safe :safe :trap) ; => :TRAP

(defclass world ()
  ((grid :initarg :grid :initform (make-hash-table :test 'equalp) :accessor grid)
   (left-bound :initarg :left-bound :reader left-bound)
   (right-bound :initarg :right-bound :reader right-bound)))

(defmethod print-object ((obj world) stream)
  (print-unreadable-object (obj stream :type t)
    (dotimes (i (right-bound obj))
      (format stream "~A" (if (eq :safe (gethash i (grid obj)))
                              #\.
                              #\^)))))

(defun out-of-bounds (world))

(defun seed-world (input)
  (let ((left-bound 0)
        (right-bound (length input))
        (grid (make-hash-table :test 'equalp)))
    (loop :for tile :across input
          :for x :from 0
          :do (setf (gethash (complex x 0) grid)
                    (if (char= tile #\.)
                        :safe
                        :trap)))

    (make-instance 'world :grid grid
                          :left-bound left-bound
                          :right-bound right-bound)))

(defun compute-row (y)
  )
(compute-row 1)
