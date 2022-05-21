(defpackage "DAY/19"
  (:use "CL"))
(in-package "DAY/19")

(defparameter +input+ 3014603)

(defun solve/1 (elf-count)
  (let ((circle (make-array elf-count :initial-element 1)))
    (flet ((find-next (start)
             (loop :for ix :from start
                   :for bounded-ix := (mod ix elf-count)
                   :until (plusp (aref circle bounded-ix))
                   :finally (return bounded-ix))))
      (loop :for current := 0 :then (find-next next)
            :for next := (find-next (1+ current))
            :until (= current next)
            :do (setf (aref circle current) (+ (aref circle current)
                                               (aref circle next))
                      (aref circle next) 0)
            :finally (return (values (1+ current)
                                     (count-if-not #'zerop circle)
                                     ;; circle
                                     ))))))

(defun solve/2 (elf-count)
  (let ((circle (make-array elf-count :initial-contents (loop :for i :from 1 :upto elf-count
                                                              :collect i))))
    (flet ((find-next (start)
             (mod (+ start
                     (floor (length circle) 2))
                  (length circle))))
      (loop :for ix := 0 :then (mod (1+ ix)
                                    (length circle))
            :until (= 1 (length circle))
            :do (let ((victim (find-next ix)))
                  (setf circle
                        (concatenate 'vector
                                     (subseq circle 0 victim)
                                     (subseq circle (1+ victim))))
                  (format t "Circle: ~A~%" circle)
                  ;; (format t "Circle length: ~A~%" (length circle))
                  )))
    circle))


;; DAY/19> (solve/2 5)
;; Circle: #(1 2 4 5)
;; Circle: #(1 2 4)
;; Circle: #(2 4)
;; Circle: #(4)
;; #(4)

(defun solve/2 (elf-count)
  (let ((circle (loop :for i :from 1 :upto elf-count
                      :collect i)
                )
        (circle-length elf-count))
    (flet ((find-next (start)
             (mod (+ start
                     (floor circle-length 2))
                  circle-length))
           (remove-elt (list index)
             (if (zerop index)
                 (setf circle (cdr circle))
                 (let* ((head (loop :for curr := list :then (cdr curr)
                                    :repeat (1- index)
                                    :finally (return curr)))
                        (next (cddr head)))
                   ;; (format t "head: ~A~%" head)
                   ;; (format t "next: ~A~%" next)
                   (cond
                     ((null (cdr head)) (rplacd head nil))
                     (t (rplacd head next)))))
             (decf circle-length)))
      (loop :for ix := 0 :then (mod (1+ ix)
                                    circle-length)
            :until (= 1 circle-length)
            :do (let ((victim (find-next ix)))
                  ;; (format t "ix: ~A. victim: ~A.~%" ix victim)
                  ;; (when (and (= ix 10)
                  ;;            (= victim 21))
                  ;;   (return-from solve/2 circle))
                  (remove-elt circle victim)
                  ;; (format t "Circle: ~A. ~A~%" circle-length circle)
                  ;; (format t "Circle length: ~A~%" (length circle))
                  )))
    circle))

;; DAY/19> (time (solve/2 +input+))
;; Evaluation took:
;;   9954.107 seconds of real time
;;   9947.374149 seconds of total run time (9947.178089 user, 0.196060 system)
;;   99.93% CPU
;;   33,779,777,359,612 processor cycles
;;   48,234,496 bytes consed

;; (155557)


;; https://www.youtube.com/watch?v=uCsD3ZGzMgE

(defun solve/1 (elf-count)
  (let* ((n (floor (log elf-count 2)))
         (l (- elf-count (expt 2 n))))
    (values n l (1+ (expt 2 l)))))

;; elfs <- 3018458 //Puzzle input
;; maxNumber <- floor(log(elfs, base = 3))
;; root <- 3^maxNumber
;; rest <- elfs-root
;; if(rest==0){
;;     Q2 <- elfs
;; } else {
;;      Q2 <- min(root,rest)+max(0,rest-root)*2
;; }
;; Q2

(defun solve/2 (elf-count)
  (let* ((max-number (floor (log elf-count 3)))
         (root (expt 2 max-number))
         (rest (- elf-count root)))
  (if (zerop rest)
      elf-count
      (+ (min root rest)
         (* 2 (max 0 (- rest root)))))))


;; def part2(n):
;;     p = 3**int(log(n-1,3))
;;     return n-p+max(n-2*p,0)

;; (defun solve/2 (n)
;;   (let ((p (expt 2 (floor (log (1- n) 3)))))
;;     (- n (+ p (max (- n (* 2 p))
;;                    0)))))
