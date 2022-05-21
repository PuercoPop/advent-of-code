(defpackage "AOC/6"
  (:use "CL"
        "SPLIT-SEQUENCE"
        "SB-CONCURRENCY"))

(in-package "AOC/6")

(defparameter +example+ #P"6.example")
(defparameter +input+ #P"6.input")

(defun read-graph (path)
  (let ((graph (make-hash-table :test 'equalp)))
    (with-open-file (in path)
      (loop :for line := (read-line in nil)
            :while line
            :do (destructuring-bind (from to)
                    (split-sequence #\) line)
                  (setf (gethash from graph) (cons to (gethash from graph nil))))))
    graph))

(defun reachable-1 (start graph)
  "Return the number of reachable nodes starting from START."
  (let ((q (make-queue)))
    (enqueue (gethash start graph) q)
    (loop :while (not (queue-empty-p q))
          :for current := (dequeue q)
          :for orbiting-planets := (gethash current graph)
          :when orbiting-planets
            :do (dolist (p orbiting-planets)
                  (enqueue p q))
            :and :count 1)))

(defun solve/1 (path)
  (let ((graph (read-graph path)))
    (loop :for planet :being :the :hash-keys :of graph
          :sum (reachable-1 planet graph))))


(read-graph +example+)
