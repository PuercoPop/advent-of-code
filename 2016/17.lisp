(defpackage "DAY/17"
  (:use "CL"))

(in-package "DAY/17")

;; If your passcode were:
;; - ihgpwlah, the shortest path would be DDRRRD.
;; - With kglvqrro, the shortest path would be DDUDRLRRUDRD.
;; - With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.

(defparameter +input+ "lpvhkcbi")

(defparameter +directions+
  '((#\U . #C( 0  1))
    (#\D . #C( 0 -1))
    (#\R . #C( 1  0))
    (#\L . #C(-1  0))))

(defun openp (n)
  (<= #xB n #xF))

;; Change this to return directions
(defun open-doors (input path)
  "Return the open doors"
  (let* ((hash (md5:md5sum-string (concatenate 'string input path)))
         (ret ())
         (up (openp (ldb (byte 4 4) (aref hash 0))))
         (down (openp (ldb (byte 4 0) (aref hash 0))))
         (left (openp (ldb (byte 4 4) (aref hash 1))))
         (right (openp (ldb (byte 4 0) (aref hash 1)))))
    (when left
      (push #\L ret))
    (when right
      (push #\R ret))
    (when up
      (push #\U ret))
    (when down
      (push #\D ret))
    ret))

(defun available-doors (point)
  (let ((ret ()))
    (let ((x (realpart point))
          (y (imagpart point)))
      (when (< 0 x)
        (push #\L ret))
      (when (< x 3)
        (push #\R ret))
      (when (> 0 y)
        (push #\U ret))
      (when (< -3 y )
        (push #\D ret))
      ret)))

(defun moves (input path point)
  (intersection (open-doors input path)
                (available-doors point)))

(defun apply-move (state direction)
  (let ((dir (cdr (assoc direction +directions+))))
    (make-state (+ (current-position state) dir)
                (concatenate 'string
                             (path state)
                             (string direction))
                (1+ (step-count state)))))

(defun apply-moves (position directions)
  (loop :for direction :in directions
        :collect (apply-move position direction)))

(defclass state ()
  ((position :initarg :position :reader current-position)
   (step-count :initarg :step-count :initform 0 :accessor step-count)
   (path :initarg :path :reader path)))

(defmethod print-object ((obj state) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A. Steps: ~A. Path: ~A" (current-position obj) (step-count obj) (path obj))))

(defun make-state (position path step-count)
  (make-instance 'state
                 :position position
                 :path path
                 :step-count step-count))

(defun solve/1 (input)
  (let* ((target #C(3 -3))
         (next (sb-concurrency:make-queue :initial-contents (list (make-state #C(0 0) "" 0)))))
    (loop
      :for current := (sb-concurrency:dequeue next)
      :for next-moves := (apply-moves current (moves input (path current) (current-position current)))
      :do (dolist (m next-moves)
            (sb-concurrency:enqueue m next))
      :until (or (= (current-position current) target)
                 (sb-concurrency:queue-empty-p next))
      :finally (return current))))

(defun solve/2 (input)
  (let* ((target #C(3 -3))
         (next (sb-concurrency:make-queue :initial-contents (list (make-state #C(0 0) "" 0))))
         (paths ()))
    (loop
      :for current := (sb-concurrency:dequeue next)
      :for next-moves := (apply-moves current (moves input (path current) (current-position current)))
      :do (dolist (m next-moves)
            (if (= (current-position m) target)
                (push m paths)
                (sb-concurrency:enqueue m next)))
      :until (sb-concurrency:queue-empty-p next))
    paths))
