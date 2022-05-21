(defpackage "DAY/06"
  (:use "CL"))

(in-package "DAY/06")

(defparameter +input+ #P"6.input")
(defparameter +example+ #P"6.example")

;; First 26 bits of an integer to a letter logior (or)
;; logcount (popcount)

(defparameter +alphabet+ "abcdefghijklmnopqrstuvwxyz")

(defun parse-answers (string)
  (loop :for char :across string
        :for pos := (position char +alphabet+ :test #'char=)
        :for num := (expt 2 pos)
        :for result := (logior 0 num) :then (logior result num)
        :finally (return result)))

(defun part-1 (pathname)
  (with-open-file (in pathname)
    (let ((group-total 0)
          (current-group 0))
      (loop :for line := (read-line in nil)
            :while line
            :do (if (string= "" line)
                    (setf group-total (+ group-total (logcount current-group))
                          current-group 0)
                    (setf current-group
                          (logior current-group (parse-answers line))))
            :finally (setf group-total (+ group-total (logcount current-group))))
      group-total)))

(defun part-2 (pathname)
  (with-open-file (in pathname)
    (let ((group-total 0)
          (current-group (1- (expt 2 27))))
      (loop :for line := (read-line in nil)
            :while line
            :do (if (string= "" line)
                    (setf group-total (+ group-total (logcount current-group))
                          current-group (1- (expt 2 27)))
                    (setf current-group
                          (logand current-group (parse-answers line))))
            :finally (setf group-total (+ group-total (logcount current-group))))
      group-total)))



#+borked
(defun part-1 (pathname)
  (with-open-file (in pathname)
    (loop :sum (loop :for line := (read-line in nil "")
                     :until (or (eql :eof line)
                                (string= "" line))
                     :for current-group := (logior 0 (parse-answers line))
                       :then (logior current-group (parse-answers line))
                     :do (format t "~A. Line: ~A~%" current-group line)
                         (return (logcount current-group))
                     :finally (return (logcount current-group))))))
(parse-answers "ac")



;; Improved version

(defun tally-group (group)
  (logcount (reduce #'logior (mapcar #'parse-answers group))))

(defun part-1* (pathname)
  (let ((groups (with-open-file (in pathname)
                  (loop :while (peek-char nil in nil)
                        :collect (loop :for line := (read-line in nil)
                                       :while line
                                       :until (string= line "")
                                       :collect line)))))
    (reduce #'+
            (mapcar #'tally-group groups))))
