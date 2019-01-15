(defpackage "AOC/23"
  (:use "CL"
        "SPLIT-SEQUENCE")
  (:export
   #:+bots+))
(in-package "AOC/23")

(defparameter +input+ #P"23.input")

(defclass pos ()
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)
   (z :initarg :z :reader z)))

(defmethod print-object ((obj pos) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "(~A,~A,~A)"
            (x obj)
            (y obj)
            (z obj))))

(defun make-pos (x y z)
  (make-instance 'pos :x x
                      :y y
                      :z z))

(defclass nanobot ()
  ((id :initarg :id :reader id)
   (pos :initarg :pos :reader pos)
   (radius :initarg :radius :reader radius)))

(defmethod print-object ((obj nanobot) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A R:~A"
            (id obj)
            (pos obj)
            (radius obj))))

(defun make-nanobot (id pos r)
  (make-instance 'nanobot :id id
                          :pos pos
                          :radius r))

(defun parse-line (i line)
  (let* ((pos-start (position #\< line))
         (pos-end (position #\> line))
         (positions (split-sequence #\, (subseq line (1+ pos-start) pos-end)))
         (radius-start (position #\= line :from-end t))
         (radius (parse-integer (subseq line (1+ radius-start)))))
    (make-nanobot i
                  (apply #'make-pos (mapcar #'parse-integer positions))
                  radius)))

(defparameter +bots+
  (with-open-file (in +input+)
    (loop :for line := (read-line in nil)
          :for i :from 1
          :while line
          :collect (parse-line i line))))

(loop :for bot :in +bots+
      :for r := (radius bot)
      :maximizing r) ; => 99885130 (27 bits, #x5F4204A)

(defparameter +strongest+
  (loop :for bot :in +bots+
        :for r := (radius bot)
        :when (= 99885130 r)
          :do (return bot))) ; => #<NANOBOT 439 #<POS (-58559428,18779440,62955743)> R:99885130>

(defun distance-from (pos-1 pos-2)
  (let ((dx (abs (- (x pos-1) (x pos-2))))
        (dy (abs (- (y pos-1) (y pos-2))))
        (dz (abs (- (z pos-1) (z pos-2)))))
    (+ dx dy dz)))

(defun solve/1 (bots beacon)
  (loop :with max-radius := (radius beacon)
    :for bot :in bots
        :count (<= (distance-from (pos beacon) (pos bot))
                   max-radius)))

(defun remove-nth (pos bots)
  (append (subseq bots 0 pos)
          (subseq bots (1+ pos))))

(defun in-range (pos bot)
  (<= (distance-from pos (pos bot))
      (radius bot)))

(defparameter +map+ (make-hash-table :test 'equalp)
  "A map from each position to how how many bots in range are there")


(defun permutations-for (distance)
  (loop :for x :from distance :downto 0
        :appending
        (loop :for y :from (- distance x) :downto 0
              :for z := (- distance x y)
              :collect (list x y z))))

(defun points-at-distance (pos distance)
  ;; First I need to compute the permutations of the distance into 3 buckets
  ;; Then collect the result of pos + permunation₁
  (loop :for (x y z) :in (permutations-for distance)
        :collect (list (+ (x pos) x)
                       (+ (y pos) y)
                       (+ (z pos) z))))

(defun annotate-map (bot)
  (loop :for pos :in (points-at-distance (pos bot)
                                         (radius bot))
        :do (setf (gethash pos +map+)
                  (1+ (gethash pos +map+ 0)))))

;; This crashes Lisp. I'm going to generate a heat map of potentially
;; interesting locations to narrow the search by sorting by bot how many other
;; bots are in their range.
(defun solve/2 (bots)
  (loop :for bot :in bots
        :do (annotate-map bot)))


;; Discard
(loop :for bot :in +bots+
      :maximizing (x (pos bot))) ; => 119269392 (27 bits, #x71BE810)

(loop :for bot :in +bots+
      :maximizing (y (pos bot))) ; => 217792276 (28 bits, #xCFB3F14)

(loop :for bot :in +bots+
      :maximizing (z (pos bot))) ; => 211595067 (28 bits, #xC9CAF3B)

(defvar +in-range+ (make-hash-table :test 'equalp))


;; This works but it is not computationally viable
(defun solve/2* (bots x-limit y-limit z-limit)
  (loop
    :for x :upto x-limit
    :do (loop
          :for y :upto y-limit
          :do (loop
                :for z :upto z-limit
                :for pos := (make-pos x y z)
                :do
                   (setf (gethash (list x y z) +in-range+)
                         (bots-in-range pos bots))))))


(defparameter +ex2-bots+
  (with-input-from-string (in "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5")
    (loop :for line := (read-line in nil)
          :for i :from 1
          :while line
          :collect (parse-line i line))))

(solve/2* +ex2-bots+ 50 50 50)

(defun solve/2 (bots)
  (loop
    :for x :upto 119269392
    :do (loop
          :for y :upto 217792276
          :do (loop
                :for z :upto 211595067
                :for pos := (make-pos x y z)
                :do
                   (setf (gethash (list x y z) +in-range+)
                         (bots-in-range pos bots))))))

(defun bots-in-range (pos bots)
  (loop :for bot :in bots
        :count (in-range pos bot)))

(defun solve/2* (bots)
  (loop :for i :from 0 :below (length bots)
        :when (= 939 (solve/1 (remove-nth i bots) (elt bots i)))
          :collect (elt bots i)))

(defun distance-from-pos (pos-1 pos-2)
  (let ((dx (abs (- (x pos-1) (x pos-2))))
        (dy (abs (- (y pos-1) (y pos-2))))
        (dz (abs (- (z pos-1) (z pos-2)))))
    (+ dx dy dz)))

(loop
  :for count :being :the :hash-values :of +in-range+
  :maximizing count )

(loop
  :for pos :being :the :hash-keys :of +in-range+
  :for count :being :the :hash-values :of +in-range+
  :when (= count 5)
    :collect pos)

