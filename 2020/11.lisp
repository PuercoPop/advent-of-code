(defpackage #:day/11
  (:use #:cl))
(in-package #:day/11)

(defparameter +input+ #P"11.input")
(defparameter +example+ #P"11.example")

(defun parse-square (char)
  (ecase char
    (#\# :ocuppied)
    (#\L :empty)
    (#\. :floor)))

(defun read-grid (pathname)
  (with-open-file (in pathname)
    (let ((initial-contents (loop :for line := (read-line in nil)
                                  :while line
                                  :collect (map 'list 'parse-square line))))
      (make-array (list (length initial-contents)
                        (length (car initial-contents)))
                  :initial-contents initial-contents))))

(defun occupiedp (square)
  (eq square :occupied))

(defun neighboors (grid x y)
  (destructuring-bind (max-x max-y)
      (array-dimensions grid)
    (loop :for (i . j) :in (list (cons (1- x) (1- y))
                                 (cons     x  (1- y))
                                 (cons (1+ x) (1- y))
                                 (cons (1- x)     y)
                                 (cons (1+ x)     y)
                                 (cons (1- x) (1+ y))
                                 (cons     x  (1+ y))
                                 (cons (1+ x) (1+ y)))
          :when (and (<= 0 i (1- max-x))
                     (<= 0 j (1- max-y)))
            :collect (aref grid i j))))

(defun next-square (grid x y)
  (let* ((current (aref grid x y))
         (neighboors (neighboors grid x y))
         (occupied-count (count-if #'occupiedp neighboors)))
    (cond ((and (eq current :empty)
                (zerop occupied-count))
           (values :occupied t))
          ((and (eq current :occupied)
                (>= occupied-count 4))
           (values :empty t))
          (t (values current nil)))))

(defun tick (grid rule)
  (let ((changedp nil)
        (next (make-array (array-dimensions grid))))
    (loop :for x :from 0 :below (first (array-dimensions grid))
          :do (loop :for y :from 0 :below (second (array-dimensions grid))
                    :for (next-square changed?) := (multiple-value-list (funcall rule grid x y))
                    :do (setf (aref next x y) next-square)
                        (when changed?
                          (setf changedp t))))
    (values next changedp)))

(defun tally (grid)
  (let ((flat-grid (make-array (array-total-size grid) :displaced-to grid)))
    (count-if #'occupiedp
              flat-grid)))

(defun solve (grid rule)
  (loop :for (next-grid changedp) := (multiple-value-list (tick grid rule))
          :then (multiple-value-list (tick next-grid rule))
        :while changedp
        ;; :do (print-grid next-grid) (format t "~%")
        :finally (return (values (tally next-grid)
                                   ;; next-grid
                                   ))))

(defun part-1 (pathname)
  (let ((grid (read-grid pathname)))
    (solve grid #'next-square)))

(defun see (grid x y dx dy)
  (destructuring-bind (max-x max-y)
      (array-dimensions grid)
    (loop :with next := nil
          :for i := (+ x dx) :then (+ i dx)
          :for j := (+ y dy) :then (+ j dy)
          :while (and (<= 0 i (1- max-x))
                      (<= 0 j (1- max-y)))
          :for current := (when (and (<= 0 i (1- max-x))
                                     (<= 0 j (1- max-y)))
                              (aref grid i j))
          :until (member current '(:occupied :empty))
          :finally (return-from see (when (member current '(:occupied :empty))
                                      current)))))

(defun visible-neighboors (grid x y)
  (let* ((north     (see grid x y  0  1))
         (northeast (see grid x y  1  1))
         (east      (see grid x y  1  0))
         (southeast (see grid x y  1 -1))
         (south     (see grid x y  0 -1))
         (southwest (see grid x y -1 -1))
         (west      (see grid x y -1  0))
         (northwest (see grid x y -1  1)))
    (remove nil (list north northeast east southeast south southwest west northwest))))

(defun next-square* (grid x y)
  (let* ((current (aref grid x y))
         (neighboors (visible-neighboors grid x y))
         (occupied-count (count-if #'occupiedp neighboors)))
    (cond ((and (eq current :empty)
                (zerop occupied-count))
           (values :occupied t))
          ((and (eq current :occupied)
                (>= occupied-count 5))
           (values :empty t))
          (t (values current nil)))))


(defun part-2 (pathname)
  (let ((grid (read-grid pathname)))
    (solve grid #'next-square*)))


(defun print-grid (grid)
  (destructuring-bind (max-x max-y)
      (array-dimensions grid)
    (loop :for x :from 0 :below max-x
          :do (loop :for y :from 0 :below max-y
                    :do (format t "~C" (ecase (aref grid x y)
                                         (:occupied #\#)
                                         (:empty #\L)
                                         (:floor #\.)))
                    :finally (format t "~%")))))
