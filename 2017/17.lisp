(defpackage #:aoc/17
  (:use #:cl))
(in-package #:aoc/17)


(defun insert (el seq position)
  (let ((left-seq (subseq seq 0 position))
        (right-seq (subseq seq position)))
    (concatenate 'vector
                 left-seq
                 (vector el)
                 right-seq)))

(defun %step (seq current-index next-el stride target-el)
  (if (> next-el target-el)
      (values seq current-index (aref seq (1+ current-index)))
      (let* ((next-index (mod (+ current-index stride)
                              (length seq)))
             (next-seq (insert next-el seq (1+ next-index))))
        (%step next-seq (1+ next-index) (1+ next-el) stride target-el))))

(defun solve-1 (step times)
  (%step (vector 0) 0 1 step times))

#+iterative
(defun solve-iter (step times)
  (loop
    :with seq := (vector 0)
    :with current-index := 0
    :with next-el := 1
    :repeat times
    :do (let* ((next-index (mod (+ current-index step)
                                (length seq)))
               (next-seq (insert next-el seq (1+ next-index))))
          (psetf seq next-seq
                 current-index (1+ next-index)
                 next-el (1+ next-el)))
    :finally (return (values seq current-index (aref seq (1+ current-index))))))

;; Part 1 -> 1173
#+puzzle-2
(solve-iter 304 50000000)
