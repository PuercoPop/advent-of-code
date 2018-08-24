(defpackage #:aoc/12
  (:use #:cl #:split-sequence))
(in-package #:aoc/12)

(defparameter +neighbors+ (make-hash-table))

(defun note-neighbors (description)
  (let ((nodes (list* (parse-integer (car description))
                      (mapcar 'parse-integer
                              (mapcar (lambda (x)
                                        (string-trim '(#\,) x))
                                      (cddr description))))))
    (dolist (n nodes)
      (setf (gethash n +neighbors+)
            (union 
             (remove n nodes)
             (gethash n +neighbors+))))))

(with-open-file (in #P"12.input")
  (loop
    :for line := (read-line in nil)
    :while line
    :do (note-neighbors (split-sequence #\Space line))))

(defun %solve/1 (current to-visit seen)
  (let* ((neighbors (remove-if (alexandria:rcurry 'member seen)
                               (gethash current +neighbors+)))
         (next-visit (set-difference
                      (union to-visit
                             neighbors)
                      seen)))
    (cond ((null next-visit)
           (values seen
                   (length seen)
                   (= (length seen)
                      (length (remove-duplicates seen)))))
          (to-visit (%solve/1 (car to-visit)
                              next-visit
                              (union (list (car to-visit))
                                    seen))))))

(defun solve/1 (initial-node)
  (%solve/1 initial-node (gethash initial-node +neighbors+) (list 0)))
#+(or)(solve/1 0)

(defun orphans (connected-nodes)
  (loop
    :for key :being :the :hash-keys :of +neighbors+
    :unless (member key connected-nodes)
      :collect key))

(defun %solve/2 (orphans seen count)
  (cond ((null orphans) count)
        (t
         (let* ((group (solve/1 (car orphans)))
                (next-seen (set-difference seen group)))
           (%solve/2 (set-difference (cdr orphans)
                                     next-seen)
                     next-seen
                     (1+ count))))))

#+(or)
(defun solve/2 (initial-node)
  (let ((group (solve/1 initial-node)))
    (%solve/2 (orphans group) group 1)))

#+(or)
(defun solve/2 ()
  (loop
    :with set-of-sets := (fset:empty-set)
    :for node :being :the :hash-keys :of +neighbors+
    :do (setf set-of-sets
              (fset:with (fset:set (solve/1 node)) set-of-sets))
    :finally (return set-of-sets)))

(defun solve/2 ()
    (loop
      :with remaining := (alexandria:hash-table-keys +neighbors+)
      :with group := nil
      :while remaining
      :do
         (incf group-count)
         (setf group (solve/1 (car remaining))
               remaining (cdr remaining))
         (setf remaining
               (set-difference remaining
                               group))
         (format t "Remaining count: ~A~%" (length remaining))
      :count t))

#+(or)(solve/2 0)

#+(or)
(with-open-file (out #P"12.pl" :direction :output :if-exists :supersede)
      (maphash (lambda (key value)
                 (dolist (n value)
                   (format out "edge(~A, ~A).~%" key n)))
               +neighbors+))
