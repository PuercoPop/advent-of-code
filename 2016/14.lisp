(defpackage "DAY/14"
  (:use "CL"
        "ALEXANDRIA"
        "SB-MD5")
  (:import-from "CRYPTO"
                "BYTE-ARRAY-TO-HEX-STRING"))
(in-package "DAY/14")

(defparameter +salt+ "abc")
(defparameter +salt+ "ahsbgdzn")

(defun hash (index)
  (byte-array-to-hex-string (md5sum-string (format nil "~A~A" +salt+ index))))

(defun hash-2 (string)
  (byte-array-to-hex-string (md5sum-string string)))

(defparameter +streteched-hashes+ (make-hash-table :test 'equal))

(defun %stretched-hash (index)
  (loop :for count :from 0 :upto 2016
        :for hash := (hash index) :then (hash-2 hash)
        :finally (return hash)))

(defun  stretched-hash (index)
  (if-let (hash (gethash index +streteched-hashes+))
    hash
    (setf (gethash index +streteched-hashes+) (%stretched-hash index))))

(defun consecutive-characters (string limit)
  (loop :with count := 1
        :with last-char := (aref string 0)
        :for char :across (subseq string 1)
        :do (if (char= last-char char)
                (incf count)
                (setf count 1))
            (when (= count limit)
              (return (values t char)))
            (setf last-char char)
        :finally (return (values nil nil))))


(defun solve-1 () 
  (loop :with count := 0
        :for index :from 0
        :do
           (multiple-value-bind (hit-1 char-1) (consecutive-characters (hash index) 3)
             (when hit-1
               (when (some (lambda (hash) (multiple-value-bind (hit-2 char-2) (consecutive-characters hash 5)
                                            (when hit-2
                                              (char= char-1 char-2))))
                           (mapcar 'hash
                                   (iota 1000 :start (1+ index))))
                 (incf count))))
           (when (= count 64)
             (return index))))

(defun solve-2 () 
  (loop :with count := 0
        :for index :from 0
        :do
           (multiple-value-bind (hit-1 char-1) (consecutive-characters (stretched-hash index) 3)
             (when hit-1
               (when (some (lambda (hash) (multiple-value-bind (hit-2 char-2) (consecutive-characters hash 5)
                                            (when hit-2
                                              (char= char-1 char-2))))
                           (mapcar 'stretched-hash
                                   (iota 1000 :start (1+ index))))
                 (incf count))))
           (when (= count 64)
             (return index))))

#|
1st case

DAY/14> (time (solve-1))
Evaluation took:
  5.668 seconds of real time
  5.612000 seconds of total run time (5.588000 user, 0.024000 system)
  [ Run times consist of 0.272 seconds GC time, and 5.340 seconds non-GC time. ]
  99.01% CPU
  14,138,266,385 processor cycles
  2,583,411,392 bytes consed

DAY/14> (time (dotimes (i 1000)  (hash 2020)))
Evaluation took:
  0.005 seconds of real time
  0.008000 seconds of total run time (0.008000 user, 0.000000 system)
  160.00% CPU
  12,450,630 processor cycles
  949,040 bytes consed

Pre memo

DAY/14> (time (dotimes (i 1000)  (stretched-hash 2020)))
Evaluation took:
  1.820 seconds of real time
  1.808000 seconds of total run time (1.800000 user, 0.008000 system)
  [ Run times consist of 0.080 seconds GC time, and 1.728 seconds non-GC time. ]
  99.34% CPU
  4,539,661,990 processor cycles
  807,326,048 bytes consed

PostMemo
Evaluation took:
  0.009 seconds of real time
  0.008000 seconds of total run time (0.008000 user, 0.000000 system)
  88.89% CPU
  22,776,762 processor cycles
  785,840 bytes consed
#|
