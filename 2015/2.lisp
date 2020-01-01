(in-package #:cl-user)

(defvar *input-file*
  #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2015/2.input")

(defun l (dimensions)
  (first dimensions))

(defun w (dimensions)
  (second dimensions))

(defun h (dimensions)
  (third dimensions))

(defun paper-for-present (present-dimensions)
  (let ((l (l present-dimensions))
        (w (w present-dimensions))
        (h (h present-dimensions)))
    (+ (* 2 l w) (* 2 l h) (* 2 w h)
       (min (* l w) (* l h) (* w h)))))

(paper-for-present '(1 1 10))

(with-open-file (in *input-file*)
  (loop :for present-dimensions := (read-line in nil 'eof)
        :until (eql present-dimensions 'eof)
        :sum (paper-for-present (mapcar #'parse-integer (split-sequence:split-sequence #\x present-dimensions)))))

(defun ribbon-for-present (present-dimensions)
  (let ((l (l present-dimensions))
        (w (w present-dimensions))
        (h (h present-dimensions)))
    (min (* 2 (+ l w))
         (* 2 (+ l h))
         (* 2 (+ w h)))))

(defun ribbon-for-bow (present-dimensions)
  (let ((l (l present-dimensions))
        (w (w present-dimensions))
        (h (h present-dimensions)))
    (* l w h)))

(with-open-file (in *input-file*)
  (loop :for present-dimensions := (read-line in nil 'eof)
        :until (eql present-dimensions 'eof)
        :sum (funcall (lambda (dimensions)
                       (+ (ribbon-for-bow dimensions)
                          (ribbon-for-present dimensions)))
                     (mapcar #'parse-integer (split-sequence:split-sequence #\x present-dimensions)))))
