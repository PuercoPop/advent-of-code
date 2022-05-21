(defpackage #:day/13
  (:use #:cl
        #:split-sequence))
(in-package #:day/13)

(defparameter +input+ #P"13.input")
(defparameter +example+ #P"13.example")

(defun read-input (pathname)
  (with-open-file (in pathname)
    (values (parse-integer (read-line in))
            (mapcar #'parse-integer
                    (remove-if (lambda (string) (string= string "x"))
                        (split-sequence #\, (read-line in)))))))

(defun earliest-time (bus-id target)
  (loop :for n :from 0
        :for time := (* bus-id n)
        :while (>= target time)
        :finally (return time)))

;; To find the bus id we need to see which is the smallest multiple
;; after the target. Then subtract from the target and multiply thy
;; the bus id.
(defun part-1 (pathname)
  (multiple-value-bind
        (target bus-ids)
      (read-input pathname)
    (let* ((ht (make-hash-table))
           (earliest-time (loop :for bus-id :in bus-ids
                                :for earliest-time := (earliest-time bus-id target)
                                :do (setf (gethash bus-id ht)
                                          earliest-time)
                                :minimizing earliest-time))
           (bus-id (loop :for bus-id :being :the :hash-keys :of ht
                         :for time :being :the :hash-values :of ht
                         :do (when (= earliest-time time)
                               (return bus-id)))))
      (values (* bus-id (- earliest-time
                           target))
              bus-id
              earliest-time
              ht))))

;; for part 2
(defun read-input* (pathname)
  ;; We want to return an alist where each pair is (bus-id . Δd)
  (let ((seq (with-open-file (in pathname)
               (read-line in)
               (split-sequence #\, (read-line in)))))
    (labels ((rec (alist seq times)
               (if (endp seq)
                   alist
                   (rec (if (string= "x" (car seq))
                            alist
                            (acons (parse-integer (car seq))
                                   times
                                   alist))
                        (cdr seq)
                        (1+ times)))))
      (nreverse (rec nil seq 0)))))

(defun initialize-counts (targets))

(defun part-2 (pathname)
  (let* ((targets (read-input* pathname))
         (counts (let ((ht (make-hash-table))) (initialize-counts ))))
    ;; So the rate of growth is different for each one. We can add the
    ;; 'base' number to avoid keeping track of with 'n' the current
    ;; number represents.
    targets))
