(defpackage "DAY/9"
  (:use "CL"))
(in-package "DAY/9")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/9.input")

(defun read-word-width (in)
  (let ((accum 0))
    (loop :for char := (read-char in nil 'eof)
          :until (char-equal char #\x)
          :do (setf accum (+ (* 10 accum) (digit-char-p char))))
    accum))

(defun read-word-times (in)
  (let ((accum 0))
    (loop :for char := (read-char in nil 'eof)
          :until (char-equal char #\))
          :do (setf accum (+ (* 10 accum) (digit-char-p char))))
    accum))

(defun read-marker (in)
  (values (read-word-width in) (read-word-times in)))

(defun read-word (in n)
  (loop :with result := ()
        :repeat n
        :for char := (read-char in nil 'eof)
        :do (push char result)
        :finally (return (coerce (reverse result) 'string))))

(defun transform-1 (in out)
  (loop :for char := (read-char in nil 'eof)
        :until (eq char 'eof)
        :do (cond ((char= #\( char)
                   ;; =>
                   (multiple-value-bind (word-width word-times)
                       (read-marker in)
                     (let ((word (read-word in word-width)))
                       (loop :repeat word-times
                             :do (write-string word out)))))
                  (t (write-char char out)))))

(with-output-to-string (out)
  (with-open-file (in +input+)
    (transform-1 in out)))

