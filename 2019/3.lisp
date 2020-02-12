(defpackage "DAY/3"
  (:use "CL"
        "SPLIT-SEQUENCE"))

(in-package "DAY/3")

(defparameter +input+ #P"3.input")

(defun make-point (x y)
  (complex x y))

(defun points-in-path (path)
  (loop :with curr := (make-point 0 0)
        :with total-steps := 0
        :with points := (make-hash-table)
        :for segment :in path
        :for (steps . Δ) := segment
        :do (loop :repeat steps
                  :do (incf total-steps)
                      (setf curr (+ curr Δ)
                            (gethash curr points) total-steps))
        :finally (return points)))

(defun intersect (points-1 points-2)
  (let ((intersections (make-hash-table)))
    (loop :for point :being :the :hash-keys :of points-1
          :when (gethash point points-2)
            :do (setf (gethash point intersections)
                      (+ (gethash point points-1)
                         (gethash point points-2))))
    intersections))

(defun distance (p)
  (+ (abs (realpart p))
     (abs (imagpart p))))

(defun read-path (path)
  (mapcar 'read-segment
          (split-sequence #\, path)))

(defun read-segment (segment)
  (let* ((direction (char segment 0))
         (steps (parse-integer (subseq segment 1)))
         (Δ (ecase direction
              (#\U #C(0 1))
              (#\D #C(0 -1))
              (#\L #C(-1  0))
              (#\R #C( 1  0)))))
    (cons steps Δ)))


(defun solve/1 (path-1 path-2)
  (let ((path-1 (read-path path-1))
        (path-2 (read-path path-2)))
    (loop :for point :being the hash-keys of (intersect (points-in-path path-1)
                                                        (points-in-path path-2))
          :minimizing (distance point))))

(solve/1 "R8,U5,L5,D3" "U7,R6,D4,L4")

(solve/1  "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          "U62,R66,U55,R34,D71,R55,D58,R83")

(solve/1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
         "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(with-open-file (in +input+)
  (let ((path-1 (read-line in))
        (path-2 (read-line in)))
    (solve/1 path-1 path-2)))

(defun solve/2 (path-1 path-2)
  (let* ((path-1 (read-path path-1))
         (path-2 (read-path path-2))
         (intersections (intersect (points-in-path path-1)
                                   (points-in-path path-2))))
    (loop :for distance :being the hash-values of intersections
          :minimizing distance)))


(solve/2  "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          "U62,R66,U55,R34,D71,R55,D58,R83")

(solve/2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
         "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(with-open-file (in +input+)
  (let ((path-1 (read-line in))
        (path-2 (read-line in)))
    (solve/2 path-1 path-2)))
