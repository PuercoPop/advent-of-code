(defpackage "DAY/18"
  (:use "CL"
        "TRIVIA"))
(in-package "DAY/18")

(defparameter +input+ "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^..")
(defparameter +test+ "..^^.")
(defparameter +test-2+ ".^^.^.^^^^")

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

(defun make-world (first-line)
  (let* ((line-length (length first-line))
         (line (make-array line-length :initial-element nil))
         (world (make-array 400000 :fill-pointer 0)))
    (loop :for cell :across first-line
          :for cell-state := (if (eq cell #\.)
                                 :safe
                                 :trap)
          :for x :from 0
          :do (setf (aref line x) cell-state))
    (vector-push line world)
    world))

(defun aref* (array ix)
  (handler-case (aref array ix)
    (sb-int:invalid-array-index-error () :safe)))

(defun compute-row (world y)
  (let* ((prev-row (aref world (1- y)))
         (row-length (length prev-row))
         (next-row (make-array row-length :initial-element nil)))
    (loop :for ix :from 0 :below row-length
          :for left-cell   := (aref* prev-row (1- ix))
          :for middle-cell := (aref* prev-row ix)
          :for right-cell  := (aref* prev-row (1+ ix))
          :for next-cell := (trap? left-cell middle-cell right-cell)
          :do (setf (aref next-row ix)
                    next-cell))
    next-row))

(defun add-row (world)
  (let ((new-row (compute-row world (length world))))
    (vector-push new-row world)
    world))

(defun build-world (input depth)
  (let ((world (make-world input)))
    (loop :repeat (1- depth)
          :do (add-row world))
    world))

(defun count-safe-tiles (world)
  (loop :for row :across world
        :sum (loop :for cell :across row
                   :count (eq cell :safe))))


(defun solve/1 (input depth)
  (let ((world (build-world input depth)))
    (count-safe-tiles world)))

(solve/1 +input+ 40) ; => 1963 (11 bits, #x7AB)

(solve/1 +input+ 400000)
