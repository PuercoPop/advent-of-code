(defpackage #:day/10
  (:use #:cl
        #:sb-concurrency))
(in-package #:day/10)

(defparameter +input+ #P"10.input")
(defparameter +example-1+ #P"10.example-1")
(defparameter +example-2+ #P"10.example-2")


;; We have to:

;; 1. Determine the highest number.
;; 2. Start from 0 looking for possible 1-3 step increments.

(defun read-bag (pathname)
  (with-open-file (in pathname)
    (let ((max 0)
          (chargers (make-hash-table)))
      (loop :for num := (read in nil)
            :while num
            :do (when (> num max)
                  (setf max num))
                (setf (gethash num chargers) t))
      (values chargers
              max))))

(defun build-path (bag max-charger)
  (let ((1-count 0)
        (2-count 0)
        (3-count 1)
        (path ()))
    (labels ((iterate (current path)
               (if (= max-charger current)
                   path
                   (let ((next (cond ((gethash (1+ current) bag)
                                      (incf 1-count)
                                      (1+ current))
                                     ((gethash (+ 2 current) bag)
                                      (incf 2-count)
                                      (+ 2 current))
                                     ((gethash (+ 3 current) bag)
                                      (incf 3-count)
                                      (+ 3 current))
                                     (t (error "Could not find viable path forward.")))))
                     (remhash current bag)
                     (iterate next
                              (cons next path)))
                   )))
      (setf path (nreverse (iterate 0 ()))))
    (values (* 1-count 3-count) 1-count 2-count 3-count path)))

(defun part-1 (pathname)
  (multiple-value-bind (bag max)
      (read-bag pathname)
    (build-path bag max)))


(defun possible-paths (current bag)
  (loop :for n :from (1+ current)
        :repeat 3
        :when (gethash n bag)
          :collect n))

;; I need to use fset to count keep a set of seen paths so I don't count them twice
;; I also need to count the max + 3
(defun count-paths (bag max)
  (setf (gethash max bag) t)
  (let ((paths (make-queue :initial-contents (possible-paths 0 bag)))
        (count 0))
    (loop :until (queue-empty-p paths)
          :do (let* ((current (dequeue paths))
                     (next (remove-if (lambda (x)
                                       (> x max))
                                      (possible-paths current bag))))
                (if (= max current)
                    (incf count))
                (dolist (n next)
                  (enqueue n paths))))
    (values count)))

;; The difference. How many times the difference happens in a row
(defun tally (diff run-length)
  (ecase diff
    (1 (ecase run-length
         (1 1)
         (2 2)
         (3 4)
         (4 7)))
    (3 run-length)))

(defun part-2 (pathname)
  (multiple-value-bind (bag max)
      (read-bag pathname)
    ;; TODO: Account for the final 3 charger
    (loop :with run-length := 0
          :with prev-value := 0
          :for n :from 1 :upto max
          :for prev-diff := (- n prev-value)
          :do (if (/= prev-value ))
          :)))

#+old
(defun part-2 (pathname)
  (multiple-value-bind (bag max)
      (read-bag pathname)
    (count-paths bag (+ 3 max))))
