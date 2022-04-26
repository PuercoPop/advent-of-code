(defpackage #:day/13
  (:use #:cl))
(in-package #:day/13)

(defparameter +input+ #P"13.input")
(defparameter +example+ #P"13.example")

(defun extract-names (line)
  (destructuring-bind (src would sign amount happiness units by sitting next to dst)
      (split-sequence:split-sequence #\Space line)
    (declare (ignore would sign amount happiness units by sitting next to))
    (list src (string-trim '(#\.) dst))))
(defun node-names (path)
  (with-open-file (in path)
    (apply #'vector
           (serapeum:nub
            (loop :for line := (read-line in nil)
                  :while line
                  :append (extract-names line))))))
(defun weights (path nodes)
  (with-open-file (in path)
    (let ((ret (make-array (list (length nodes) (length nodes)) :initial-element 0))
          (entries (loop :for line := (read-line in nil)
                         :while line
                         :collect (parse-line line))))
      (loop :for (src w dst) :in entries
            :for sidx := (position src nodes :test #'string=)
            :for didx := (position dst nodes  :test #'string=)
            :do (setf (aref ret sidx didx) w))
      ret)))
(defun parse-line (line)
  (destructuring-bind (from would sign amount happiness units by sitting next to dest)
      (split-sequence:split-sequence #\Space line)
    (declare (ignore would happiness units by sitting next to))
    (let* ((sign (if (string-equal "gain" sign) 1 -1))
          (amount (parse-integer amount))
           (total (* sign amount))
           (to (subseq dest 0 (1- (length dest)))))
      (list from total to))))


(defun tally-position (seats weights)
  (loop :with len := (length seats)
        :for ix :from 0
        :for n :across seats
        :for l := (mod (1- ix) len)
        :for r := (mod (1+ ix) len)
        :sum (aref weights ix l)
        :sum (aref weights ix r)))
(tally-position #("David" "Alice" "Bob" "Carol") (weights +example+ (node-names +example+)))

(defun seat-arrangements (nodes)
  (labels ((iter (arrangement seated to-seat)
             (cond ((= (length (car seated))
                       (length nodes))
                    (iter (cons (car seated)
                                seated)
                      (cdr seated)
                      to-seat))))
           )
    (iter nil nil nodes))
  )

(defun solve/1 (in)
  (let* ((nodes (node-names in))
         (weights (weights in nodes))
         (arragenments (seat-arrangements nodes))
         )
    (loop :for arrangement :in arrangements
          :maximize (tally-position arrangements weights))))
(solve/1 +example+)
