(defpackage "DAY/07"
  (:use "CL"
        "SPLIT-SEQUENCE")
  (:import-from "SERAPEUM"
                "DICT"
                "MERGE-TABLES"))
(in-package "DAY/07")

(defparameter +input+ #P"7.input")
(defparameter +example+ #P"7.example")
(defparameter +sample-rule+ (with-open-file (in +example+)
  (read-line in)))


(defclass edge ()
  ((weight :initarg :weight :initform 0 :reader edge-weight)
   (to :initarg :to :reader edge-destination)))

(defmethod print-object ((obj edge) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "(~A, ~A)" (edge-destination obj) (edge-weight obj))))

(defun parse-tail (tail)
  (let* ((tail (string-trim '(#\. #\Space) tail))
         (pivot (position #\Space tail))
         (end-pos (position #\Space tail :from-end t))
         (count (subseq tail 0 pivot)))
    (make-instance 'edge :to (subseq tail (1+ pivot) end-pos)
                         :weight (parse-integer count))))

(defun parse-rule (line)
  (let* ((head-end (subseq line 0 (search "bags" line)))
         (head (string-trim '(#\Space)
                            head-end))
         (tail-start (+ (search "contain" line)
                        (length "contain")))
         (tail (unless (search "no other bags" line)
                 (mapcar #'parse-tail
                         (split-sequence #\, (subseq line tail-start))))))
    (values head tail)))

(defun read-rules (pathname)
  (let ((hash (make-hash-table :test #'equalp)))
    (with-open-file (in pathname)
      (loop :for line := (read-line in nil)
            :while line
            :for (head tails) := (multiple-value-list (parse-rule line))
            :unless (null tails)
            :do (setf (gethash head hash) tails)))
    hash))

(defun reachable? (graph starting-bag target-bag)
  ;; Expand all
  (labels ((iterate (visited to-visit)
             (if (null to-visit)
                 visited
                 (let* ((current (car to-visit))
                        (next-bags (mapcar #'edge-destination (gethash current graph)))
                        (next-to-visit (set-difference next-bags visited :test #'string=)))
                   (iterate (union visited (list current) :test #'string=) next-to-visit)))))
    (let ((possible-bags (iterate (list starting-bag) (mapcar #'edge-destination (gethash starting-bag graph)))))
      (values (find target-bag possible-bags :test #'string=)
              possible-bags))))

(defun part-1 (pathname)
  (let ((graph (read-rules pathname))
        (target-bag "shiny gold"))

    (loop :for bag :being :the :hash-key :of graph
          :when (string/= target-bag bag)
            :count (reachable? graph bag target-bag))))
