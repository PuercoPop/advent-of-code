(defpackage "DAY/1"
  (:use "CL"))

(in-package "DAY/1")

(defparameter +input+ #P"1.input")

(defun fuel-req (mass)
  (- (floor mass 3) 2))

;; 1st.
(with-open-file (in +input+)
  (loop :for mass := (read in nil nil)
        :while mass
        :sum (fuel-req mass)))

(defun fixed-fuel-req (mass)
  (let ((fuel-req (fuel-req mass)))
    (if (< fuel-req 0)
        0
        (+ fuel-req (fixed-fuel-req fuel-req)))))

;; 2nd.
(with-open-file (in +input+)
  (loop :for mass := (read in nil nil)
        :while mass
        :sum (fixed-fuel-req mass)))
