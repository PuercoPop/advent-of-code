(defpackage #:day/9
  (:use #:cl))
(in-package #:day/9)


(defparameter +input+ #P"9.input")
(defparameter +example+ #P"9.example")

;; The queue should have always maintain the same number of elements.
(defun make-queue (contents)
  (sb-concurrency:make-queue :initial-contents contents))

(defun push-window (queue el)
  (sb-concurrency:dequeue queue)
  (sb-concurrency:enqueue el queue)
  queue)

(defun check (target-num window)
  "Checks if two numbers present in the WINDOW add up to TARGET-NUM."
  (let ((seen (make-hash-table)))
    (loop :for num :in (sb-concurrency:list-queue-contents window)
          :for pair-presentp := (gethash (- target-num num) seen)
          :do (setf (gethash num seen) t)
              (when pair-presentp
                (return-from check t)))))

(defun part-1 (preamble pathname)
  (with-open-file (in pathname)
    (let ((queue (make-queue (loop :repeat preamble
                                   :collect (parse-integer (read-line in))))))
      (loop :for line := (read-line in nil)
            :while line
            :for target := (parse-integer line)
            :do (unless (check target queue)
                  (return-from part-1 target))
                (push-window queue target)))))


;; 105950735

(defun total-sum (queue)
  (reduce #'+ (sb-concurrency:list-queue-contents queue)))

(defun find-set (target in)
  (let ((queue (sb-concurrency:make-queue)))
    (loop :for line := (read-line in nil)
          :while line
          :for num := (parse-integer line)
          :for total-sum := (total-sum queue)
          :do (when (= total-sum target)
                (return-from find-set queue))
              (sb-concurrency:enqueue num queue)
              (loop :until (<= (total-sum queue) target)
                    :do (sb-concurrency:dequeue queue)))))

(defun find-min-max (queue)
  (let* ((contents (sb-concurrency:list-queue-contents queue))
         (first (car contents))
         (min first)
         (max first))
    (loop :for n :in contents
          :do (cond ((>= min n) (setf min n))
                    ((<= max n) (setf max n))))
    (values min max)))

(defun part-2 (target-num pathname)
  (with-open-file (in pathname)
    (let ((set (find-set target-num in)))
      (multiple-value-bind (min max)
          (find-min-max set)
        (+ min max)))))
