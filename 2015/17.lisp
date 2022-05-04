(defpackage "DAY/17"
  (:use "CL"
        "SERAPEUM"))
(in-package "DAY/17")

;; One difficulty posed by this problem is that we want to be able to
;; distinguish between two distinct 5 liter jugs. To do so we to track
;; the indices for the containers instead of the values themselves..

(defparameter +input+
  (apply #'vector
         (with-open-file (in #P"17.input")
           (sort (loop :for line := (read-line in nil)
                       :while line
                       :collect (parse-integer line))
                 #'>))))
(defparameter +example+ #(20 15 10 5 5))

(defun solve/1 (input target)
  (let ((combinations (make-hash-table :test #'equalp))
        (q (queue ())))
    (loop :while (consp (qlist q))
          :for current := (progn
                            (format t "Len: ~A. Q: ~A~%" (qlen q) (qlist q))
                            (deq q))
          :for sum := (reduce #'+ (mapcar (lambda (ix)
                                            (aref input ix))
                                          current))
          :do
             (if (= sum target)
                  (incf (gethash (sort current #'<) combinations 0))
                  (loop :for ix :from 0
                        :for jug :across input
                        :when (and (<= (+ sum jug) target)
                                   (not (find ix current)))
                          :do (enq (cons ix (mapcar #'identity current))
                                  q))
                  ))
    ;; (loop :for k :being :the :hash-keys :of combinations :count k)
    combinations
    ))

;; (assert (= 25 (solve/1 +example+ 25)) nil)
;; (solve/1 +input+ 150)
