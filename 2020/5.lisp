(defpackage "DAY/05"
  (:use "CL"))

(in-package "DAY/05")

(defparameter +input+ #P"5.input")

;; 128 rows on the plane (numbered 0 through 127).
;; 8 columns of seats on the plane (numbered 0 through 7)

"BFFFBBFRRR"

(defun split-instruction (line)
  (cons (subseq line 0 7)
          (subseq line 7)))

(defun %row (inst min max)
  ;; (format t "inst: ~A. min: ~A. max: ~A~%" inst min max)
  (if (string= "" inst)
      (progn
        (assert (= min max) () "min max differ.")
        min)
      (let* ((dir (char inst 0))
             (rest (subseq inst 1))
             (size (1+ (- max min)))
             (step (/ size 2))
             (min (if (char= #\B dir)
                      (+ min step)
                      min))
             (max (if (char= #\B dir)
                      max
                      (- max step))))
        (%row rest min max))))

(defun row (inst)
  (%row inst 0 127))


;; 0 1 2 3 .. 7
;;

;; BFFFBBFRRR: row 70, column 7, seat ID 567.
;; FFFBBBFRRR: row 14, column 7, seat ID 119.
;; BBFFBBFRLL: row 102, column 4, seat ID 820.

(defun %column (inst min max)
  ;; (format t "inst: ~A. min: ~A. max: ~A~%" inst min max)
  (if (string= "" inst)
      (progn
        (assert (= min max) () "min max differ.")
        min)
      (let* ((dir (char inst 0))
             (rest (subseq inst 1))
             (size (1+ (- max min)))
             (step (/ size 2))
             (min (if (char= #\R dir)
                      (+ min step)
                      min))
             (max (if (char= #\R dir)
                      max
                      (- max step))))
        (%column rest min max))))

(defun column (inst)
  (%column inst 0 7))

(defun seat-id (row col)
  (+ (* 8 row)
     col))

(defun part-1 (pathname)
  (with-open-file (in pathname)
    (loop :for line := (read-line in nil)
          :while line
          :for (row-inst . col-inst) := (split-instruction line)
          :for row := (row row-inst)
          :for col := (column col-inst)
          :for seat-id := (seat-id row col)
          :maximizing seat-id)))


(with-open-file (in +input+)
  (sort (apply #'vector
          (loop :for line := (read-line in nil)
                :while line
                :for (row-inst . col-inst) := (split-instruction line)
                :for row := (row row-inst)
                :for col := (column col-inst)
                :for seat-id := (seat-id row col)
                :collect seat-id))
        #'<=))

(defun part-2 (pathname)
  (with-open-file (in pathname)
    (let ((seat-ids (sort (apply #'vector
                                 (loop :for line := (read-line in nil)
                                       :while line
                                       :for (row-inst . col-inst) := (split-instruction line)
                                       :for row := (row row-inst)
                                       :for col := (column col-inst)
                                       :for seat-id := (seat-id row col)
                                       :collect seat-id))
                          #'<=))
          (prev-seat-id 12))
      (loop :for seat-id :across seat-ids
            :do
               (when (/= seat-id (1+ prev-seat-id))
                 (format t "seat not found. seat-id: ~A~%" seat-id)
                 (return-from part-2 (values seat-id prev-seat-id)))
               (setf prev-seat-id seat-id)))))

(defun part-2* (pathname)
  (labels ((rec (seat-ids low high)
             (let* ((min-seat (aref seat-ids low))
                    (max-seat (aref seat-ids high))
                    (mid-point (floor (/ (+ high low)
                                         2)))
                    (mid-seat (aref seat-ids mid-point))
                    (target-mid-seat (/ (+ max-seat min-seat)
                                        2)))
               (cond ((= (- high low) 1) (values target-mid-seat low high))
                     ((< mid-seat target-mid-seat )
                      ;; This means that the missing seat is to the right (upper half).
                      (rec seat-ids mid-point high))
                     (t ;; otherwise we assume it is to the left (lower half).
                       (rec seat-ids low mid-point))))))
    (with-open-file (in pathname)
             (let ((seat-ids (sort (apply #'vector
                                          (loop :for line := (read-line in nil)
                                                :while line
                                                :for (row-inst . col-inst) := (split-instruction line)
                                                :for row := (row row-inst)
                                                :for col := (column col-inst)
                                                :for seat-id := (seat-id row col)
                                                :collect seat-id))
                                   #'<=)))
               (rec seat-ids 0 (1- (length seat-ids)))))))
