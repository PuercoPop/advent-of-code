(defpackage "DAY/1"
  (:use "CL"))
(in-package "DAY/1")

(defparameter +map+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/day1-input.txt")

(defun direction (move)
  (ecase (aref move 0)
    (#\R :right)
    (#\L :left)))

(defun distance (move)
  (parse-integer (subseq move 1)))

(defvar *moves*)
(let ((input (with-open-file (in +map+)
               (mapcar (alexandria:curry 'string-trim '(#\Space)) (split-sequence:split-sequence #\, (read-line in))))))
  (sb-int:collect ((moves))
    (dolist (move input)
      ;; (moves (cons (direction move) (distance move)))
      (moves move))
    (setf *moves* (moves))))

(defun step-orientation (current-orientation move)
  (ecase current-orientation
    (:up (ecase (direction move)
           (:right :right)
           (:left :left)))
    (:right (ecase (direction move)
           (:right :down)
           (:left :up)))
    (:down (ecase (direction move)
           (:right :left)
           (:left :right)))
    (:left (ecase (direction move)
           (:right :up)
           (:left :down)))))

(defun step-position (current-orientation current-position move)
  ;; Sign
  (let ((sign (ecase current-orientation
                ((:up :right) '+)
                ((:down :left) '-)))
        (axis (ecase current-orientation
                ((:up :down) 'y)
                ((:right :left) 'x))))
    (destructuring-bind (x y) current-position
      (ecase current-orientation
        (:up (list x (+ y (distance move))))
        (:right (list (+ x  (distance move)) y))
        (:down (list x (- y (distance move))))
        (:left (list (- x (distance move)) y))))))

(defun final-position (moves)
  (let ((position (list 0 0))
        (orientation :up))
    (dolist (move moves)
      (setf orientation (step-orientation orientation move))
      (setf position (step-position orientation position move)))
    (values position orientation)))

(defun position= (p1 p2)
  (and (eql (first p1) (first p2))
       (eql (second p1) (second p2))))

;; El problema con esta solucion es que no considera los pasos lugares intermedios por los que cruze
(defun final-position/2-fail-1 (moves)
  (let ((position (list 0 0))
        (orientation :up)
        (past-positions ()))
    (dolist (move moves)
      (setf orientation (step-orientation orientation move))

      (push position past-positions)
      (setf position (step-position orientation position move))

      (when (member position past-positions :test 'position=)
        (return-from final-position/2-fail-1 (values position past-positions orientation))))
    (error "No match found")))

(defun final-position/2 (moves)
  (let ((position (list 0 0))
        (orientation :up)
        (previous-position nil)
        (past-positions ()))
    (dolist (move moves)
      (setf orientation (step-orientation orientation move))
      (psetf position (step-position orientation position move)
             previous-position position)

      (setf past-positions (append (intermediate-positions postion previous-position) past-positions))
      
      (push position past-positions)
      (when (member position past-positions :test 'position=)
        (return-from final-position/2 (values position past-positions orientation)))
      

      )
    (error "No match found")))

(defun compute-distance (end start))

(final-position *moves*)
(compute-distance (final-position *moves*) '(0 0))
