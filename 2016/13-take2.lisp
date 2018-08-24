(defpackage "DAY/13"
  (:use "CL")
  (:import-from "SB-CONCURRENCY"
                "MAKE-QUEUE"
                "ENQUEUE"
                "DEQUEUE"
                "QUEUE-EMPTY-P"))
(in-package "DAY/13")

(defparameter +target-room+ #C(7 4))
(defparameter +fav-number+ 10)

(defparameter +target-room+ #C(31 39))
(defparameter +fav-number+ 1358)

(defun x (coordinate)
  (realpart coordinate))

(defun y (coordinate)
  (imagpart coordinate))

(defun polynomial (coordinate)
  (let ((x (x coordinate))
        (y (y coordinate)))
    (+ (* x x)
       (* 3 x)
       (* 2 x y)
       (* y y)
       y)))

(defun wallp (coordinate)
  (oddp (logcount (+ +fav-number+ (polynomial coordinate)))))

(defclass move ()
  ((coordiante :initarg :coordinate :reader move-coordinate)
   (step :initarg :step :initform 0 :reader move-step :documentation "The current step number.")))

(defmethod print-object ((obj move) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "(~A, ~A)" (move-coordinate obj) (move-distance obj))))

(defun main-1 (favorite-number target-coord)
  (loop
    :with queue := (make-queue :initial-contents '(#C(0 0)))
    :for current-position := (dequeue queue)
    :until (= current-position coord)
    :do (map nil (lambda (x)
                   (enqueue x moves-heap))
             (valid-moves current-position))))
