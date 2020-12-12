(defpackage #:day/12
  (:use #:cl)
  (:shadow #:position))
(in-package #:day/12)

(defparameter +input+ #P"12.input")
(defparameter +example+ #P"12.example")

(defclass ship ()
  ((direction :initform 1 :accessor ship-direction)
   (position :initform #C(0 0) :accessor position)))

(defclass waypoint ()
  ((position :initform #C(10 1) :accessor position)))

(defparameter +directions+ #(:north :east :south :west))

(defun update-direction (ship count)
  (setf (ship-direction ship)
        (mod (+ (ship-direction ship) count)
             (length +directions+))))

(defun move-north (ship distance)
  (setf (position ship) (+ (position ship) (complex 0 distance))))

(defun move-east (ship distance)
  (setf (position ship) (+ (position ship) (complex distance 0))))

(defun move-south (ship distance)
  (setf (position ship) (+ (position ship) (complex 0 (- distance)))))

(defun move-west (ship distance)
  (setf (position ship) (+ (position ship) (complex (- distance) 0))))

(defun move-forward (ship distance)
  (let ((direction (aref +directions+ (ship-direction ship))))
    (ecase direction
      (:north (move-north ship distance))
      (:east (move-east ship distance))
      (:south (move-south ship distance))
      (:west (move-west ship distance)))))

(defun turn-right (ship angle)
  (ecase angle
    (90 (update-direction  ship 1))
    (180 (update-direction ship 2))
    (270 (update-direction ship 3))
    (-90 (update-direction  ship -1))
    (-180 (update-direction ship -2))
    (-270 (update-direction ship -3))))

(defun move (ship instruction num)
  (ecase instruction
    (:north (move-north ship num))
    (:east  (move-east  ship num))
    (:south (move-south ship num))
    (:west  (move-west  ship num))
    (:forward (move-forward ship num))
    (:left (turn-right ship (- num)))
    (:right (turn-right ship num))))

(defun parse-instruction (line)
  (values (ecase (char line 0)
            (#\N :north)
            (#\S :south)
            (#\E :east)
            (#\W :west)
            (#\F :forward)
            (#\L :left)
            (#\R :right))
          (parse-integer (subseq line 1))))

(defun manhattan-distance (point)
  (+ (abs (realpart point))
     (abs (imagpart point))))

(defun part-1 (pathname)
  (with-open-file (in pathname)
    (let ((ship (make-instance 'ship)))
      (loop :for line := (read-line in nil)
            :while line
            :do (multiple-value-bind
                      (move num)
                    (parse-instruction line)
                  (move ship move num)))
      (values (manhattan-distance (position ship))
              (position ship)
              ship))))

(defun move-towards-waypoint (ship waypoint times)
  (setf (position ship) (+ (position ship) (* times (position waypoint)))))

(defun rotate (position times)
  (let ((new-position (complex (imagpart position)
                      (- (realpart position)))))
    (if (= times 1)
        new-position
        (rotate new-position (1- times)))))

(defun rotate-waypoint (waypoint angle)
  (ecase angle
    (90 (setf (position waypoint)
              (rotate (position waypoint) 1)))
    (180 (setf (position waypoint)
               (rotate (position waypoint) 2)))
    (270 (setf (position waypoint)
               (rotate (position waypoint) 3)))
    (-90 (setf (position waypoint)
               (rotate (position waypoint) 3)))
    (-180 (setf (position waypoint)
                (rotate (position waypoint) 2)))
    (-270 (setf (position waypoint)
                (rotate (position waypoint) 1)))))

(defun part-2 (pathname)
  (with-open-file (in pathname)
    (let ((ship (make-instance 'ship))
          (waypoint (make-instance 'waypoint)))
      (loop :for line := (read-line in nil)
            :while line
            :do (multiple-value-bind
                      (move num)
                    (parse-instruction line)
                  (ecase move
                    (:north (move-north waypoint num))
                    (:east (move-east waypoint num))
                    (:south (move-south waypoint num))
                    (:west (move-west waypoint num))
                    (:forward (move-towards-waypoint ship waypoint num))
                    (:left (rotate-waypoint waypoint (- num)))
                    (:right (rotate-waypoint waypoint num)))))
      (values (manhattan-distance (position ship))
              (position ship)
              (position waypoint)))))
