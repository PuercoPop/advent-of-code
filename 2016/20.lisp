(defpackage "DAY/20"
  (:use "CL"
        "ALEXANDRIA"
        "SPLIT-SEQUENCE"))
(in-package "DAY/20")

(defparameter +input+ #P"20.input")

(defclass blacklist ()
  ((lower-bound :initarg :lower :reader lower-bound)
   (upper-bound :initarg :upper :reader upper-bound)))

(defmethod print-object ((obj blacklist) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A, ~A" (lower-bound obj) (upper-bound obj))))

(defun merge-rules/2 (rule-1 rule-2)
  (when (>= (upper-bound rule-1)
            (1- (lower-bound rule-2)))
    (make-instance 'blacklist
                   :lower (lower-bound rule-1)
                   :upper (max (upper-bound rule-1)
                               (upper-bound rule-2)))))

#+smoke
(let ((rule-1 (make-instance 'rule :lower 4
                                   :upper 7))
      (rule-2 (make-instance 'rule :lower 5 :upper 8)))
  (merge-rules/2 rule-1 rule-2))

(defun merge-rules (rules)
  (let ((next (make-array (length rules) :fill-pointer 0))
        (remaining (subseq rules 1)))
    (vector-push (aref rules 0) next)
    (loop :until (zerop (length remaining))
          :for left := (aref next (1- (length next)))
          :for right := (aref remaining 0)
          :for merged := (merge-rules/2 left right)
          :do
             (if merged
                  (setf (aref next (1- (length next)))
                        merged)
                  (vector-push right next))
             (setf remaining (subseq remaining 1)))
    next))

(with-open-file (in +input+)
  (defparameter +rules+
    (let* ((rules (loop :for line := (read-line in nil)
                        :while line
                        :collect (destructuring-bind (lower upper)
                                     (mapcar 'parse-integer
                                             (split-sequence #\- line))
                                   (make-instance 'blacklist
                                                  :lower lower
                                                  :upper upper))))
           (sorted-rules (sort rules
                               (lambda (r1 r2)
                                 (< (lower-bound r1)
                                    (lower-bound r2)))))
           (rule-vector (coerce sorted-rules 'vector)))
      (merge-rules rule-vector))))

;; First Part
(1+ (upper-bound (aref  +rules+ 0))) ; => 23923783 (25 bits, #x16D0C47)

;; Second part
(loop :for rule :across +rules+
      :sum (1+ (- (upper-bound rule)
                  (lower-bound rule)))) ; => 4294967171 (32 bits, #xFFFFFF83)

(1+ (- 4294967295 4294967171)) ; => 125 (7 bits, #x7D, #o175, #b1111101)

#+smoke
(let* ((rule-1 (make-instance 'blacklist :lower 0
                                   :upper 2))
       (rule-2 (make-instance 'blacklist :lower 4 :upper 7))
       (rule-3 (make-instance 'blacklist :lower 5 :upper 8))
       (rules (merge-rules  (vector rule-1 rule-2 rule-3))))
  (loop :for rule :across rules
      :sum (1+  (- (upper-bound rule)
                 (lower-bound rule)))))
