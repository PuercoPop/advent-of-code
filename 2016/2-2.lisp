(defpackage "DAY/2-1"
  (:use "CL"
        "ALEXANDRIA"
        "TRIVIA")
  (:shadow "STEP"))
(in-package "DAY/2-1")

(defparameter +code+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/day2-input.txt")
(defparameter +position+ (list -2 0))
(defparameter +max-radius+ 2)

(defun position->number (position)
  (ematch position
    ((list  0  2) 1)
    ((list -1  1) 2)
    ((list  0  1) 3)
    ((list  1  1) 4)
    ((list -2  0) 5)
    ((list -1  0) 6)
    ((list  0  0) 7)
    ((list  1  0) 8)
    ((list  2  0) 9)
    ((list -1 -1) 10)
    ((list  0 -1) 11)
    ((list  1 -1) 12)
    ((list  0 -2) 13)))

(macrolet ((define-direction (name x-transform y-transform)
             (with-gensyms (position x y x′ y′)
               `(defun ,name (,position)
                  (destructuring-bind (,x ,y) ,position
                    (let ((,x′ (funcall ,x-transform ,x))
                          (,y′ (funcall ,y-transform ,y)))
                      (if (< +max-radius+ (+ (abs ,x′) (abs ,y′)))
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

(solve (read-map +code+))
(solve '("ULL"
         "RRDDD"
         "LURDL"
         "UUUUD"))
