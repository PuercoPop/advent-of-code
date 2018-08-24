(defpackage #:aoc/10
  (:use #:cl
        #:split-sequence)
  (:export
   #:hash-knot))
(in-package #:aoc/10)


;; C-j to eval and open inspector


(defclass knot-hash ()
  ((list :initarg :list
         :initform (make-array 256 :initial-contents (loop :for n :from 0 :upto 255 :collect n))
         :reader knot-list)
   (current-position :initarg :current-position
                     :initform 0
                     :accessor current-position)
   (skip-size :initarg :skip-size
              :initform 0
              :accessor skip-size)))

(defmethod print-object ((obj knot-hash) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "P: ~A S: ~A XS: ~A"
            (current-position obj)
            (skip-size obj)
            (knot-list obj))))

(defparameter +input+ (with-open-file (in #P"10.input")
  (mapcar 'parse-integer (split-sequence #\, (read-line in)))))

(defun swap (seq ix iy)
  (let ((temp (aref seq ix)))
    (setf (aref seq ix)
          (aref seq iy)
          (aref seq iy)
          temp)))

(defun reverse-knot (knot length)
  (dotimes (i (floor (/ length 2)))
    (let ((ix (mod (+ (current-position knot) i)
                   (length (knot-list knot))))
          (iy (mod (- (+ (current-position knot) length)
                      (1+ i))
                   (length (knot-list knot)))))
      (swap (knot-list knot) ix iy))))

(defun step/1 (knot length)
  (reverse-knot knot length)
  (setf (current-position knot)
        (mod (+ (current-position knot) length (skip-size knot))
             (length (knot-list knot))))
  (incf (skip-size knot))
  knot)

(defun solve/1 (knot lengths)
  (loop :for length :in lengths
        :do (step/1 knot length))
  (values knot
          (subseq (knot-list knot) 0 2)
          (apply '* (coerce (subseq (knot-list knot) 0 2)
                            'list))))

(defparameter +input-2+
    (with-open-file (in #P"10.input")
      (append
       (map 'list 'char-code (read-line in))
       (list 17 31 73 47 23))))

(defun solve/2 (knot lengths)
  (dotimes (i 64)
    (loop :for length :in lengths
          :do (step/1 knot length)))
  (loop
    :for i :from 0 :below 16
    :collect (reduce
              'logxor
              (subseq (knot-list knot)
                      (* i 16)
                      (+ (* i 16) 16)))))

#+(or)(solve/1 +lengths+)


(solve/2 (make-instance 'knot-hash)
         (append (map 'list 'char-code "1,2,3")
                 (list 17 31 73 47 23)))

(defun hash-knot (string)
  (solve/2 (make-instance 'knot-hash) (append (map 'list 'char-code string)
                                              (list 17 31 73 47 23))))
;; => (3E FB E7 8A 8D 82 F2 99 79 3 1A 4A A0 B1 6A 9D)
;; 3efbe78a8d82f2997931a4aa0b16a9d
;; 3efbe78a8d82f29979031a4aa0b16a9d

