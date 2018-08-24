(defpackage "AOC/9"
  (:use #:cl #:split-sequence))
(in-package #:aoc/9)

(defclass group ()
  ((contents :initarg :contents :reader group-contents)
   (score :initform 0 :accessor group-score)))

(defclass garbage ()
  ((contents :initarg :contents :reader garbage-contents)))

(defmethod print-object ((obj garbage) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "with ~{~A~^,~}" (garbage-contents obj))))

(defun read-garbage (in)
  (let ((contents ()))
    (loop :for char := (read-char in)
          :until (char= char #\>)
          :do (if (char= char #\!)
                  (read-char in)
                  (setf contents (cons char contents))))
    (make-instance 'garbage :contents contents)))

(defun read-group (in)
  (let ((contents ()))
    (loop :for char := (read-char in)
          :until (char= char #\})
          :do (cond ((char= char #\{ ) (setf contents (cons (read-group in)
                                                            contents))
                     (when (char= #\, (peek-char nil in nil #\?))
                       (read-char in)))
                    ((char= char #\<) (setf contents (cons (read-garbage in)
                                                            contents))
                     (when (char= #\, (peek-char nil in nil #\?))
                       (read-char in)))
                    (t (error "read-group expectations violated."))))
    (make-instance 'group
                   :contents contents)))

(with-input-from-string (in "<!!>")
  (read-garbage in))

(defparameter +street+
  (with-open-file (in #P"9.input")
    (loop :for char := (read-char in nil)
          :while char
          :collect (cond ((char= char #\{) (prog1 (read-group in)
                                             (when (char= #\, (peek-char nil in nil #\?))
                                               (read-char in))))
                    ((char= char #\<) (prog1 (read-garbage in)
                                             (when (char= #\, (peek-char nil in nil #\?))
                                               (read-char in))))
                    (t (error "main: expectations violated."))))))

(defgeneric group? (obj))
(defmethod group? ((obj group))
  t)
(defmethod group? (obj)
  nil)

(defgeneric garbage? (obj))
(defmethod garbage? ((obj garbage))
  t)
(defmethod garbage? (obj)
  nil)

(defun subgroups (group)
  (remove-if-not 'group?
                 (group-contents group)))

(defun has-subgroups? (group)
  (not (zerop (length (subgroups group)))))

(defun score-top-level-group (group)
  (score-group group 1))

(defun score-group (group score)
  (setf (group-score group) score)
  (when (has-subgroups? group)
    (mapcar (lambda (g)
              (score-group g (1+ score)))
            (subgroups group))))

(defun score-street (street)
  (loop :for group :in street
        :do (score-top-level-group group)))

(defun count-group (group)
  (if (has-subgroups? group)
      (+ (group-score group)
         (loop :for group :in (subgroups group)
               :sum (count-group group)))
      (group-score group)))

(defun count-score (street)
  (loop :for group :in street
        :sum (count-group group)))

;; 2

(defgeneric count* (object))
(defmethod count* ((garbage garbage))
  (length (garbage-contents garbage)))

(defmethod count* ((group group))
  (if (zerop (length (group-contents group)))
      0
      (apply '+ (mapcar 'count* (group-contents group)))))

(defun has-garbage? (group)
  (not (zerop (length (remove-if-not 'garbage? (group-contents group))))))

(defun score-street (street)
  (loop :for group :in street
        :sum (count* group)))
