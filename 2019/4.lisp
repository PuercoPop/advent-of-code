(defpackage "DAY/4"
  (:use "CL"
        "ALEXANDRIA"
        "SERAPEUM"
        "SPLIT-SEQUENCE"))

(in-package "DAY/4")

(defparameter +input+ "136818-685979")

(defun %decreasing-p (num prev)
  (if (zerop num)
      t
      (multiple-value-bind (next digit)
          (floor num 10)
        (when(<= digit prev)
          (%decreasing-p next digit)))))

(defun decreasing-p (num)
  (%decreasing-p num 9))

(defun %double-digit-p (num prev)
  (unless (zerop num)
    (multiple-value-bind (next digit)
        (floor num 10)
      (if (= digit prev)
          t
          (%double-digit-p next digit)))))

(defun double-digit-p (num)
  (multiple-value-bind (next digit)
      (floor num 10)
    (%double-digit-p next digit)))

(defun valid-password-p (num)
  (and (decreasing-p num)
       (double-digit-p num)))

(defun solve/1 (low high)
  (loop :for n :from low :upto high
        :when (valid-password-p n)
          :count 1))

(defun %only-double-digit-p (num prev count)
  (if (zerop num)
      (= count 1)
      (multiple-value-bind (next digit)
          (floor num 10)
        (cond  ((= digit prev) (%only-double-digit-p next digit (1+ count)))
               ((= count 1) t)
               (t (%only-double-digit-p next digit 0))))))

(defun only-double-digit-p (num)
  (multiple-value-bind (next digit)
      (floor num 10)
    (%only-double-digit-p next digit 0)))

(defun valid-password-p* (num)
  (and (decreasing-p num)
       (only-double-digit-p num)))

(solve/1 136818 685979)

(defun solve/2 (low high)
  (loop :for n :from low :upto high
        :when (valid-password-p* n)
          :count 1))
