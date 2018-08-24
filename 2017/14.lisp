(defpackage #:aoc/14
  (:use #:cl
        #:aoc/10))

(in-package #:aoc/14)

(defparameter +input+ "hxtvlmkl")

(hash-knot "flqrgnkx-0") ; => (205 9 148 73 249 130 127 6 0 44 107 96 215 167 146 222)

(defun count-used (xs)
  (loop :for 2h :in xs
        :sum (logcount 2h)))

(count-used (hash-knot "flqrgnkx-0")) ; => 74 (7 bits, #x4A, #o112, #b1001010)

(defun solve/1 (base-string)
  (loop :for pad :from 0 :below 128
        :sum (count-used (hash-knot (format nil "~A-~A" base-string pad)))))

#+(or)(solve/1 "flqrgnkx")
;; => 8108

(defun build-array (base-string)
  (let ((arr (make-array '(128 128) :element-type 'bit :initial-element 0)))
    (loop :for row :from 0 :below 128
          :do (fill-row arr row (hash-knot (format nil "~A-~A" base-string row))))
    arr))

(defun fill-row (arr row knot-xs)
  (loop :for ix :from 0 :by 8
        :for 2H :in knot-xs
        :do (setf (aref arr row ix)        (ldb (byte 1  7) 2H)
                  (aref arr row (+  1 ix)) (ldb (byte 1  6) 2H)
                  (aref arr row (+  2 ix)) (ldb (byte 1  5) 2H)
                  (aref arr row (+  3 ix)) (ldb (byte 1  4) 2H)
                  (aref arr row (+  4 ix)) (ldb (byte 1  3) 2H)
                  (aref arr row (+  5 ix)) (ldb (byte 1  2) 2H)
                  (aref arr row (+  6 ix)) (ldb (byte 1  1) 2H)
                  (aref arr row (+  7 ix)) (ldb (byte 1  0) 2H)))
  arr)

#+(or)
(fill-row (make-array 128 :element-type 'bit :initial-element 0) '(205 9 148 73 249 130 127 6 0 44 107 96 215 167 146 222))



;; For flqrgnkx 1242 are present

(defun solve/2 (base-string)
  (let ((grid (build-array base-string)))
    grid)
  )
