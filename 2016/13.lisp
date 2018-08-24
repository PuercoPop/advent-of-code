(defpackage "DAY/13"
  (:use "CL"
        "ALEXANDRIA"
        "MAP-SET")
  (:import-from "SB-CONCURRENCY"
                "MAKE-QUEUE"
                "ENQUEUE"
                "DEQUEUE"
                "QUEUE-EMPTY-P"))
(in-package "DAY/13")


;; Queue

(defun make-queue ()
  (make-instance 'cl-heap:priority-queue))

(defun enqueue (queue item)
  (cl-heap:enqueue queue item 1))

(defun dequeue (queue)
  (cl-heap:dequeue queue))

(defun empty-queue-p (queue)
  (cl-heap:empty-queue queue))


;; Code

(defparameter +target-room+ #C(7 4))
(defparameter +fav-number+ 10)

(defparameter +target-room+ #C(31 39))
(defparameter +fav-number+ 1358)

(defparameter *past-rooms* ())



(defun x (coordinate)
  (realpart coordinate))

(defun y (coordinate)
  (imagpart coordinate))

(defclass move ()
  ((coordiante :initarg :coordinate :reader move-coordinate)
   (distance :initarg :distance :initform 0 :reader move-distance)))

(defmethod print-object ((obj move) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "(~A, ~A)" (move-coordinate obj) (move-distance obj))))

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

(defun invalid-room-p (coodinate)
  (or (wallp coodinate)
      (minusp (x coodinate))
      (minusp (y coodinate))))

(defun next-moves (move)
  (let ((coordinate (move-coordinate move))
        (distance (move-distance move)))
    (mapcar (lambda (c)
              (make-instance 'move :coordinate c :distance (1+ distance)))
            (remove-if 'invalid-room-p
                       (mapcar (curry '+ coordinate)
                               '(          #C( 0 -1)
                                 #C(-1  0)           #C( 1  0)
                                           #C( 0  1)))))))

(defun optimal-direction (coordinate &optional (destination +target-room+))
  (let ((v (- destination coordinate)))
    (atan (y v)
          (x v))))

(defun rank-moves (position &rest moves)
  (apply '<
         (mapcar (lambda (x)
                   (abs (- (optimal-direction position)
                           (optimal-direction (move-coordinate x) position))))
                 moves)))

(defun sort-moves (moves position)
  (sort moves (curry 'rank-moves (move-coordinate position))))

(defun ranked-next-moves (position)
  (sort-moves (next-moves position) position))

(defun pruned-next-moves (position past-positions)
  (remove-if (rcurry 'visitedp past-positions)
             (ranked-next-moves position)))

(defun visitedp (move past-positions)
  (ms-member-p past-positions (move-coordinate move)))

#| Search Strategy

Cases

Current Move at the same position than the target, TO.
=> Return Current Move

Moves queue is empty and there are no new possible moves from the current position
=> Return nil

Default

- queue new moves from the current-position
- Update current from the first item of the Moves Queue


|#

(defun minimum-moves (&key (from (error "Required argument: FROM.")) (to (error "Required argument: TO.")))
  (let* ((current (make-instance 'move :coordinate from :distance 0))
         (queue (make-queue))
         (past-positions (make-map-set)))
    (loop
      (cond
        ((null current)
         (return :not-found))
        ((= (move-coordinate current) to)
         (return current))
        (t (map nil (curry 'enqueue queue) (pruned-next-moves current past-positions))
           (ms-insert past-positions (move-coordinate current))
           (setf current (dequeue queue)))))))

(defun minimum-moves (&key (from (error "Required argument: FROM.")) (to (error "Required argument: TO.")))
  (let* ((current (make-instance 'move :coordinate from :distance 0))
         (queue (make-queue))
         (past-positions (make-map-set)))
    (loop :for index :from 0
     :do  (cond
        ((null current)
         (return (values :not-found index queue past-positions)))
        ((= (move-coordinate current) to)
         (return (values current index past-positions)))
        (t (map nil (curry 'enqueue queue) (pruned-next-moves current past-positions))
           (ms-insert past-positions (move-coordinate current))
           (setf current (dequeue queue)))))))

;; Trial
(minimum-moves :from #C(1 1) :to #c(3 4))

;; 1
(minimum-moves :from #C(1 1) :to +target-room+)

(defun pruned-next-moves-2 (position past-positions max-distance)
  (remove-if (curry '< max-distance)
             (remove-if (rcurry 'visitedp past-positions)
                        (ranked-next-moves position))
             :key 'move-distance))

;; Mas la inicial y la última
(defun reachable-spaces-moves (&key (from (error "Required argument: FROM.")) (range (error "Required argument: TO.")))
  (let* ((current (make-instance 'move :coordinate from :distance 0))
         (queue (make-queue))
         (past-positions (make-map-set)))
    (loop
      (cond
        ((null current)
         (return :not-found))
        (t (map nil (curry 'enqueue queue) (pruned-next-moves-2 current past-positions range))
           (ms-insert past-positions (move-coordinate current))
           (setf current (dequeue queue)))))
    (values current past-positions)))

(reachable-spaces-moves :from #C(1 1) :range 50)


;; Visualizations
(defun draw-map (past-positions &optional (max-x 40) (max-y 40))
  (declare (ignorable past-positions))
  (flet ((chat-at (position)
           (cond ((and (wallp position)
                       (ms-member-p past-positions position)) #\E)
                 ((wallp position) #\#)
                 ((= +target-room+ position) #\X)
                 ((ms-member-p past-positions position) #\v)
                 (t #\.))
           ))
    (loop :for x :from 0 :upto max-x
          :do
             (loop :for y :from 0 :upto max-y
                   :do (format t "~A" (chat-at (complex x y))))
             (format t "~%"))))
