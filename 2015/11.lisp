(defpackage #:day/11
  (:use #:cl))

(in-package #:day/11)

(defparameter +input+ "cqjxjnds")

;;; Generating the passwords
;;
;; Incrementing is just like counting with numbers: xx, xy, xz, ya,
;; yb, and so on. Increase the rightmost letter one step; if it was z,
;; it wraps around to a, and repeat with the next letter to the left
;; until one doesn't wrap around.

;; Test case xz and xzz

(defun next-char (char)
  (let* ((next-char-code (1+ (char-code char)))
         (wrap-around-p (< 122 next-char-code))
         (next-char-code (if wrap-around-p
                             (- next-char-code
                                (1+ (- (char-code #\z)
                                       (char-code #\a))))
                             next-char-code))
         (next (code-char next-char-code)))
    (values next wrap-around-p)))

(defun %next-input (input ix)
  (multiple-value-bind (next-char wrap-around-p)
      (next-char (aref input ix))
    (setf (aref input ix) next-char)
    (if wrap-around-p
        (%next-input input (1- ix))
        input)))

(defun next-input (input)
  (let* ((input (copy-seq input)))
    (%next-input input (1- (length input)))))


;;; Password validation
;;
;; Rule 1
;;
;; Passwords must include one increasing straight of at least three
;; letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip
;; letters; abd doesn't count.

(defun flushp (input)
  (loop :for char :across input
        :for a := #\Space :then b
        :for b := #\Space :then c
        :for c := char
        :for a-code := (char-code a)
        :for b-code := (char-code b)
        :for c-code := (char-code c)
        :when (and (= a-code (1- b-code))
                   (= b-code (1- c-code)))
          :count 1))

;; Rule 2
;;
;; Passwords may not contain the letters i, o, or l, as these letters
;; can be mistaken for other characters and are therefore confusing.

(defun only-allowed-chars-p (input)
  (notany (lambda (x)
            (or (char= x #\i)
                (char= x #\o)
                (char= x #\l)))
          input))

(assert (only-allowed-chars-p "cde"))
(assert (not (only-allowed-chars-p "cdie")))

;; Rule 3
;;
;; Passwords must contain at least two different, non-overlapping
;; pairs of letters, like aa, bb, or zz.

;; Bug because it doesn't enforce non-overlapping
;; (defun third-rule-p (input)
;;   (cl-ppcre:scan "/(.)\1.*?(.)\2/" input))

(defun chomp (xs)
  (let* ((char (char xs 0))
         (count (position char xs :test #'char/= :start 1)))
    (values (cons char (or count (length xs)))
            (subseq xs (or count (length xs))) )))

(defun third-rule-p (input)
  (labels ((iter (in successive-count)
             (cond ((>= successive-count 2) t)
                   ((zerop (length in)) nil)
                   (t (multiple-value-bind (m rest)
                          (chomp in)
                        (iter rest (+ successive-count (if (< 1 (cdr m))
                                                           1
                                                           0))))))))
    (iter input 0)))

(defun valid-password-p (input)
  (and (plusp (flushp input))
       (only-allowed-chars-p input)
       (third-rule-p input)))

(assert (not (valid-password-p "hijklmmn")))
(assert (not (valid-password-p "abbceffg")))
(assert (not (valid-password-p "abbcegjk")))

(defun solve/1 (input)
  (loop :for password := (next-input input) :then (next-input password)
        :until (valid-password-p password)
        :finally (return password)))

(solve/1 +input+) ; => "cqjxxyzz"
