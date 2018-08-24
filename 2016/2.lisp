(defpackage "DAY/2"
  (:use "CL"
        "ALEXANDRIA"
        "TRIVIA")
  (:shadow "STEP"))
(in-package "DAY/2")

(defparameter +code+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/day2-input.txt")
(defparameter +max-coordinate+ 1)
#|
            Keypad Co-ordinates

      +---------+----------+----------+
      |    1    |    2     |    3     |
      | (-1, 1) | ( 0, 1)  | ( 1, 1)  |
      +---------+----------+----------+
      |    4    |    5     |    6     |
      | (-1, 0) | ( 0, 0)  | ( 1, 0)  |
      +---------+----------+----------+
      |    7    |    8     |    9     |
      | (-1,-1) | ( 0,-1)  | ( 1,-1)  |
      +---------+----------+----------+

|#

(defun position->number (position)
  (ematch position
    ((list -1  1) 1)
    ((list  0  1) 2)
    ((list  1  1) 3)
    ((list -1  0) 4)
    ((list  0  0) 5)
    ((list  1  0) 6)
    ((list -1 -1) 7)
    ((list  0 -1) 8)
    ((list  1 -1) 9)))

(defparameter +position+ (list 0 0) "The initial-position.")

(macrolet ((define-direction (name x-transform y-transform)
             (with-gensyms (position x y x′ y′)
               `(defun ,name (,position)
                  (destructuring-bind (,x ,y) ,position
                    (let ((,x′ (funcall ,x-transform ,x))
                          (,y′ (funcall ,y-transform ,y)))
                      (if (or (< +max-coordinate+ (abs ,x′))
                              (< +max-coordinate+ (abs ,y′)))
                          ,position
                          (list ,x′ ,y′))))))))
  (define-direction up 'identity '1+)
  (define-direction down 'identity '1-)
  (define-direction right '1+ 'identity)
  (define-direction left '1- 'identity))

(defun op->fun (char)
  (ecase char
    (#\U #'up)
    (#\D #'down)
    (#\R #'right)
    (#\L #'left)))

(defun read-map (file)
  (with-open-file (in file)
    (loop :for line := (read-line in nil 'eof)
          :until (eq line 'eof)
          :collect line)))

(defun solve (instructions)
  (labels ((step (position instructions code)
             (cond ((endp instructions) (reverse code))
                   (:otherwise
                    (let ((final-position (funcall (reduce 'compose (reverse (map 'list 'op->fun (car instructions)))) position)))
                      (step final-position (cdr instructions) (cons (position->number final-position) code))))))) 
    (let ((position +position+)
          (code ()))
      (step position instructions code))))

(solve (read-map +code+)) ; => (4 8 5 8 4)
(solve '("ULL"
         "RRDDD"
         "LURDL"
         "UUUUD")) ; => (1 9 8 5)
