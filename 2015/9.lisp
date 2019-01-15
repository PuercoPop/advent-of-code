(defpackage "DAY9"
  (:use "CL"))
(in-package "DAY9")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2015/9.input")

(defclass graph ()
  ((cities :initform () :accessor cities)
   (distance :initform () :accessor distance)))

(defparameter +graph+ (make-instance 'graph))

(defun ensure-city (city)
  (unless (member city (cities +graph+))
    (setf (cities +graph+)
          (cons city (cities +graph+)))))

(defun add-route (city-a city-b distance)
  (ensure-city city-a)
  (ensure-city city-b)
  (setf (distance +graph+)
        (acons (list city-a city-b) distance (distance +graph+))))


(defun parse-line (line)
  (let ((first-space (position #\Space line :test 'char=))
        (to (search " to " line))
        (= (position #\= line :test 'char=)))
    (values (intern (string-upcase (subseq line 0 first-space)))
            (intern (string-upcase (subseq line (+ to 4) (1- =))))
            (parse-integer (subseq line (+ 2 =))))))
(search " to " "Norrath to Straylight = 115")
(parse-line "Norrath to Straylight = 115")

(defun load-graph ()
  (with-open-file (in +input+)
    (loop :for line := (read-line in nil)
          :while line
          :do (multiple-value-bind (a b d) (parse-line line)
                (add-route a b d)))))

(defun from (city-a city-b)
  (let* ((route (list city-a city-b))
         (reverse-route (reverse route))
         (routep (lambda (x)
                  (or (equal x route)
                      (equal x reverse-route)))))
    (loop :for (pair . distance) :in (distance +graph+)
          :when (funcall routep pair)
            :do (return distance))
    (error "Impossible state")))

(defun compute-distance (route)
  "Route: A list of city names."
  (labels ((iter (route total)
             (if (endp (cdr route))
                 total
                 (iter (cdr route) (+ (from (car route) (cadr route))
                                      total)))))
    (iter route 0)))


(defun permute (list)
  (if list
      (mapcan #'(lambda (x)
                  (mapcar #'(lambda (y) (cons x y))
                          (permute (remove x list))))
              list)
      '(()))) ; else

(defun all-routes (cities)
  (if cities
    (mapcan #'(lambda (x)
		(mapcar #'(lambda (y) (cons x y))
			(all-routes (remove x cities))))
	    cities)
    '(())
    ))

(mapcar (lambda (y) (cons (car (cities +graph+))
                          y))
        (cdr (cities +graph+)))

(defun all-routes (cities)
  (car cities _))

(loop :for route :in (all-routes (cities +graph+))
      :minimizing (compute-distance route)) ; => 251 (8 bits, #xFB, #o373, #b11111011)

(loop :for route :in (all-routes (cities +graph+))
      :maximize (compute-distance route)) ; => 898 (10 bits, #x382)
