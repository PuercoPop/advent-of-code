(defpackage #:aoc/13
  (:use #:cl
        #:split-sequence
        #:cl-ppcre))

(in-package #:aoc/13)

#+(or)(declaim (optimize (speed 3)
                       (safety 1)))

(defun trim-world (world)
  (subseq world
          0
          (1+ (position-if 'zerop world :from-end t ))))

(defun read-world (pathname)
  (let ((world (make-array 120 :initial-element 0)))
    (with-open-file (in pathname)
      (loop
        :for line := (read-line in nil)
        :while line
        :collect (register-groups-bind ((#'parse-integer layer)
                                        (#'parse-integer depth))
                     ("(\\d+): (\\d+)" line)
                   (setf (aref world layer)
                         depth)))
      (trim-world world))))


(defun calculate-sentry-position (tick max-depth)
  (let ((mod (mod tick (1- max-depth))))
    (if (evenp (floor (/ tick (1- max-depth))))
        mod
        (- (1- max-depth) mod))))

(defun solve/1 (world)
  (loop
    :for sentry-position :from 0
    :for tick :from 0
    :for layer :across world
    :when (and layer (zerop (calculate-sentry-position tick layer)))
      :sum (* sentry-position (aref world sentry-position))))

(defun succesful-run (starting-tick world)
  (loop
    :for sentry-position :from 0
    :for tick :from 0
    :for layer :across world
    :never (and (not (zerop layer))
                (zerop (calculate-sentry-position (+ starting-tick tick)
                                                  layer)))))

(defun solve/2 (world)
  (declare (type vector world))
  (loop :for starting-tick :from 0
        :when (succesful-run (the fixnum starting-tick) world)
              :return starting-tick))

#+(or)
(let ((world (read-world #P"13.input")))
  (time (solve/2 world)))

;; Pre optimize

;; Evaluation took:
;;   1.658 seconds of real time
;;   1.660000 seconds of total run time (1.660000 user, 0.000000 system)
;;   [ Run times consist of 0.028 seconds GC time, and 1.632 seconds non-GC time. ]
;;   100.12% CPU
;;   4,136,873,629 processor cycles
;;   260,212,608 bytes consed
