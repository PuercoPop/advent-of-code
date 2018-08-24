(defpackage "DAY/5-2"
  (:use "CL"
        "SB-MD5"))

(in-package "DAY/5-2")

(defparameter +input+ "ugkcyxxp")

(defun byte-array-to-hex-string (bytevec)
  (format nil "~(~{~2,'0X~}~)" (coerce bytevec 'list)))

(defun make-candidate (input index)
  (md5sum-string (format nil "~A~A" input index)))

(defun hitp (md5)
  "Return true if the md5 is a hit"
  (and (zerop (aref md5 0))
       (zerop (aref md5 1))
       (> 8 (aref md5 2))))

(defun digit-pos (candidate)
  (values (aref candidate 2)
          (ash (aref candidate 3) -4)))

(defun -1? (x)
  (eql x -1))

(defun solve (input)
  (let ((result (make-array 8 :initial-element -1)))
    (loop
      :for index :from 0
      :for candidate := (make-candidate input index)
      :until (every #'null (map 'list #'-1? result))
      :when (hitp candidate)
        :do
            (multiple-value-bind
                  (pos digit) (digit-pos candidate)
              (when (eql (aref result pos) -1)
                (setf (aref result pos) digit))))
    result))

(solve +input+) ;; #(8 0 6 E 9 B B 4) Equivocada
(solve "abc") ; Da #(E B E 6 8 E 5 4) pero deberia 05ace8e3
FFC030E5
;; String Version
;; Evaluation took:
;;   28.035 seconds of real time
;;   28.096000 seconds of total run time (27.992000 user, 0.104000 system)
;;   [ Run times consist of 1.524 seconds GC time, and 26.572 seconds non-GC time. ]
;;   100.22% CPU
;;   69,928,801,117 processor cycles
;;   12,902,513,424 bytes consed
