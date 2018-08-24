(defpackage #:aoc/17
  (:use #:cl))
(in-package #:aoc/17)

(defun solve/1 (step times)
  (let ((seq (vector 0))
        (current-index 0)
        (next-elt 1)
        (next-index))
    (dotimes (i times)
      (setf next-index (mod (+ current-index step)
                            (length seq)))
      (if (= (1+ next-index) (length seq))
          (setf seq (concatenate 'vector seq `#(,next-elt)))
          (setf seq (concatenate 'vector
                                 (subseq seq 0 next-index)
                                 `#(,next-elt)
                                 (subseq seq next-index))))
      (incf next-elt)
      (setf current-index next-index))
    (values seq current-index)))

;; Not 135
