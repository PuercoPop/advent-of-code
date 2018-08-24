(defpackage "DAY/1-2"
  (:use "CL")
  (:shadow "STEP"))
(in-package "DAY/1-2")

(defparameter +map+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/day1-input.txt")

(defvar *moves*)
(flet ((direction (move)
         (ecase (aref move 0)
           (#\R :right)
           (#\L :left)))
       (distance (move)
         (parse-integer (subseq move 1))))

(let ((input (with-open-file (in +map+)
               (mapcar (alexandria:curry 'string-trim '(#\Space)) (split-sequence:split-sequence #\, (read-line in))))))
  (sb-int:collect ((moves))
    (dolist (move input)
      (moves (cons (direction move) (distance move))))
    (setf *moves* (moves)))))

(defun direction (move)
  (car move))

(defun distance (move)
  (cdr move))

(defun position= (p1 p2)
  (and (eql (first p1) (first p2))
       (eql (second p1) (second p2))))

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

(defun step-position (position orientation move)
  (destructuring-bind (x y) position
    
    (values (cond ((and (eq :up orientation)
                        (eq :right (direction move)))
                   (list (1+ x) y))
                  ((and (eq :up orientation)
                        (eq :left (direction move)))
                   (list (1- x) y))
                  ((and (eq :right orientation)
                        (eq :right (direction move)))
                   (list x (1- y)))
                  ((and (eq :right orientation)
                        (eq :left (direction move)))
                   (list x (1+ y)))
                  ((and (eq :down orientation)
                        (eq :right (direction move)))
                   (list (1- x) y))
                  ((and (eq :down orientation)
                        (eq :left (direction move)))
                   (list (1+ x) y))
                  ((and (eq :left orientation)
                        (eq :right (direction move)))
                   (list x (1+ y)))
                  ((and (eq :left orientation)
                        (eq :left (direction move)))
                   (list x (1- y)))
                  (:otherwise (error "Impossible state reached.")))
            ;; New move
            (cons (direction move) (1- (distance move))))))

(defun final-position (moves)
  (let ((position (list 0 0))
        (orientation :up))
    (labels ((step (position orientation move remaining-moves previous-positions)
               (cond ((member position previous-positions :test 'position=) (return-from final-position position))
                     ((and (endp remaining-moves)
                         (zerop (distance move))) (error "No match found."))
                     ((zerop (distance move))
                      ;; =>
                      (step position (step-orientation orientation move) (car remaining-moves) (cdr remaining-moves) previous-positions))
                     (:otherwise (multiple-value-bind (new-position new-move) (step-position position orientation move)
                                   (step new-position orientation new-move remaining-moves (cons position previous-positions)))))))
      (step position orientation (car moves) (cdr moves) nil))))


(defun compute-distance (end start)
  (declare (ignore start)) ; Assume origin
  (apply '+ (mapcar 'abs end)))

(final-position *moves*)
(compute-distance (final-position *moves*) '(0 0))
