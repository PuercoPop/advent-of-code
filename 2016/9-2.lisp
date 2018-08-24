(defpackage "DAY/9-2"
  (:use "CL"
        "SPLIT-SEQUENCE"
        "ALEXANDRIA"))
(in-package "DAY/9-2")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/9.input")

(defparameter +som+ (aref "(" 0))
(defparameter +eom+ (aref ")" 0))

(defun marker-start-p (string)
  (char-equal +som+ (aref string 0)))

(defun read-marker (string)
  (let ((eof-pos (position +eom+ string)))
    (append (mapcar 'parse-integer (split-sequence #\x (subseq string 1 eof-pos)))
            (list (subseq string (1+ eof-pos))))))
#\
#\
\

;; "X(8x2)(3x3)ABCY"
(defun %count-characters (string count)
  (cond ((string= "" string) count)
        ((marker-start-p string)
         ;; =>
         (destructuring-bind (eat-num times rest) (read-marker string)
           (%count-characters (subseq rest eat-num)
                              (+ count
                                 (* times
                                    (%count-characters (subseq rest 0 eat-num)
                                                       0))))))
        (t (%count-characters (subseq string 1) (1+ count)))))

(defun count-characters (string)
  (%count-characters string 0))


(count-characters "X(8x2)(3x3)ABCY") ;; 20
(count-characters "(27x12)(20x12)(13x14)(7x10)(1x12)A") ;; 241920
(count-characters "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") ;; 445
(count-characters (alexandria:read-file-into-string +input+)) ; => 10943094568 (34 bits, #x28C425F28)
