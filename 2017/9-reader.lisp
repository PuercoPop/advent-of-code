(defpackage "AOC/9"
  (:use "CL"))
(in-package "AOC/9")

(defclass group ()
  ((contents :initarg :contents :reader group-contents)
   (score :initform 0 :accessor group-score)))

(defclass garbage ()
  ((contents :initarg :contents :reader garbage-contents)))

(defmethod print-object ((obj garbage) stream)
  (format stream "<~{~A~}>" (garbage-contents obj)))

(defun garbage-reader (stream char)
  (declare (ignore char))
  (format t "foo~%")
  (make-instance 'garbage
                 :contents (read-delimited-list #\> stream t)))

(set-macro-character #\<
                     'garbage-reader)

(with-input-from-string (in "<abc>")
  (read in))
