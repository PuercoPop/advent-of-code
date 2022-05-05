(defpackage "DAY/18"
  (:use "CL"))
(in-package "DAY/18")

(defparameter +input+ #P"18.input")
(defparameter +example+ #P"18.example")

(defun solve/1 (size times path)
  (let ((curr (make-array (list (+ 2 size) (+ 2 size)) :element-type '(integer 0 1) :initial-element 0))
        (next (make-array (list (+ 2 size) (+ 2 size)) :element-type '(integer 0 1) :initial-element 0)))
    (with-open-file (in path)
      (loop :for line := (read-line in nil)
            :for x :from 1
            :while line
            :do (loop :for ch :across line
                      :for y :from 1
                      :do (when (char= ch #\#)
                            (setf (aref curr x y) 1)))))
    (loop :repeat times
          :do (loop :for x :from 1 :upto size
                    :do (loop :for y :from 1 :upto size
                              :do (let ((neighbor-count (+ (aref curr (1- x) (1+ y))
                                                           (aref curr     x  (1+ y))
                                                           (aref curr (1+ x) (1+ y))
                                                           (aref curr (1+ x)     y)
                                                           (aref curr (1+ x) (1- y))
                                                           (aref curr     x  (1- y))
                                                           (aref curr (1- x) (1- y))
                                                           (aref curr (1- x)     y))))
                                    (setf (aref next x y)
                                          (ecase (aref curr x y)
                                            (1 (if (or (= 2 neighbor-count)
                                                         (= 3 neighbor-count))
                                                     1
                                                     0))
                                            (0 (if (= 3 neighbor-count)
                                                          1
                                                          0)))))))
              ;; (draw next size)
              ;; (format t "~%")
          (psetf curr next
                 next curr))
    (loop :for x :from 1 :upto size
          :sum (loop :for y :from 1 :upto size
                     :unless (zerop (aref curr x y))
                       :count 1))))

(defun draw (arr size)
  (loop :for x :from 1 :upto size
        :do (loop :for y :from 1 :upto size
                  :for ch := (if (zerop (aref arr x y))
                                 #\.
                                 #\#)
                  :do (format t "~C" ch))
        (format t "~%")))

(defun solve/2 (size times path)
  (let ((curr (make-array (list (+ 2 size) (+ 2 size)) :element-type '(integer 0 1) :initial-element 0))
        (next (make-array (list (+ 2 size) (+ 2 size)) :element-type '(integer 0 1) :initial-element 0)))
    (with-open-file (in path)
      (loop :for line := (read-line in nil)
            :for x :from 1
            :while line
            :do (loop :for ch :across line
                      :for y :from 1
                      :do (when (char= ch #\#)
                            (setf (aref curr x y) 1)))))
    (setf (aref curr    1    1) 1
          (aref curr    1 size) 1
          (aref curr size    1) 1
          (aref curr size size) 1)
    (loop :repeat times
          :do (loop :for x :from 1 :upto size
                    :do (loop :for y :from 1 :upto size
                              :do (let ((neighbor-count (+ (aref curr (1- x) (1+ y))
                                                           (aref curr     x  (1+ y))
                                                           (aref curr (1+ x) (1+ y))
                                                           (aref curr (1+ x)     y)
                                                           (aref curr (1+ x) (1- y))
                                                           (aref curr     x  (1- y))
                                                           (aref curr (1- x) (1- y))
                                                           (aref curr (1- x)     y))))
                                    (setf (aref next x y)
                                          (ecase (aref curr x y)
                                            (1 (if (or (= 2 neighbor-count)
                                                         (= 3 neighbor-count))
                                                     1
                                                     0))
                                            (0 (if (= 3 neighbor-count)
                                                          1
                                                          0)))))))
                  (setf (aref next    1    1) 1
                        (aref next    1 size) 1
                        (aref next size    1) 1
                        (aref next size size) 1)
              ;; (draw next size)
              ;; (format t "~%")
          (psetf curr next
                 next curr))
    (loop :for x :from 1 :upto size
          :sum (loop :for y :from 1 :upto size
                     :unless (zerop (aref curr x y))
                       :count 1))))
