(defpackage "DAY4"
  (:use "CL"
        "ALEXANDRIA"
        "TRIVIA"
        "TRIVIA.PPCRE")
  (:shadow "STEP"))
(in-package "DAY4")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/day4-input.txt")

(defun process-line (line)
  (let* ((last-dash (position #\- line :from-end t))
         (head (subseq line 0 last-dash))
         (tail (subseq line (1+ last-dash))))
    (append (process-head head) (process-tail tail))))

(defun process-head (head)
  (list head))

(defun process-tail (tail)
  (let* (([ (position #\[ tail))
         (sector-id (subseq tail 0 [))
         (checksum (subseq tail (1+ [) (1- (length tail)))))
    (list sector-id checksum)))

(defun build-frequency-table (head)
  (let ((table (make-hash-table :test 'equalp)))
    (loop :for char :across head
          :unless (char= char #\-)
          :do
             (setf (gethash char table) (1+ (gethash char table 0))))
    table))

(defun checksum (frequency-table)
  (labels ((step (table index max-count xs)
             (cond ((> index max-count) (coerce xs 'string))
                   ((> (count index table :key 'cdr) 0)
                    ;; =>
                    (let ((char-bag (sort (mapcar 'car (remove-if-not (lambda (el)
                                                                        (eql index (cdr el))) table))
                                          'char<)))
                      (step table (1+ index) max-count (append char-bag xs))))
                   (t (step table (1+ index) max-count xs)))))
    (at-most 5 (step (hash-table-alist frequency-table) 1 (car (sort (hash-table-values frequency-table) '>)) ()))))

(defun at-most (n xs)
  (subseq xs 0 (min n (length xs))))

(defun real-room-p (line)
  (destructuring-bind (head sector-id checksum) (process-line line)
    (values (string-equal (checksum (build-frequency-table head)) checksum)
            sector-id)))

(with-open-file (in +input+)
  (loop :for line := (read-line in nil 'eof)
        :until (eq line 'eof)
        :for (roomp sector-id) := (multiple-value-list (real-room-p line))
        :when roomp
          :sum (parse-integer sector-id))) ; => 173787 (18 bits, #x2A6DB)

(defparameter +alphabet+ "abcdefghijklmnopqrstuvwxyz")

(defun rotate-char (char n)
  (let ((index (position char +alphabet+)))
    (if index 
        (aref +alphabet+ (mod (+ index n) (length +alphabet+)))
        #\Space)))

(defun room-name (line)
  (when (real-room-p line)
    (destructuring-bind (head sector-id checksum) (process-line line)
      (map 'string (lambda (c) (rotate-char c (parse-integer sector-id))) head))))
;; "northpole object storage"
(with-open-file (in +input+)
  (loop :for line := (read-line in nil 'eof)
        :until (eq line 'eof)
        :for (roomp sector-id) := (multiple-value-list (real-room-p line))
        :when roomp
          :collect (cons (room-name line) sector-id)))

(with-open-file (in +input+)
  (loop :for line := (read-line in nil 'eof)
        :until (eq line 'eof)
        :for (roomp sector-id) := (multiple-value-list (real-room-p line))
        :when (and roomp (string-equal "northpole object storage" (room-name line)))
          :collect (cons (room-name line) sector-id)))
