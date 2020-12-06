(defpackage "DAY/8"
  (:use "CL"))

(in-package "DAY/8")

;; Santa's list is a file that contains many double-quoted string
;; literals, one on each line.

;; The only escape sequences used are:
;;
;; - \\, which represents a single backslash.
;; - \", which represents a lone  double-quote character.
;; - \x plus two hexadecimal characters,  which represents a single character with that ASCII code.

;; Q1: what is the number of characters of code for string literals
;; minus the number of characters in memory for the values of the
;; strings in total for the entire file?

(defparameter +input+ #P"8.input")
(defparameter +sample+ #P"8.sample")

(defun read-string (line)
  (flet ((read-hexcode (in)
           (read-char in)
           (+ (* 10 (digit-char-p (read-char in)
                                  16))
              (digit-char-p (read-char in)
                            16))))
    (with-input-from-string (in line)
      (read-char in)
      (coerce  (loop :for char := (read-char in nil nil)
                        :for next-char := (peek-char nil in nil nil)
                        :while next-char
                        :if (char= char #\\)
                          :collect (cond ((char= next-char #\\)
                                          (read-char in)
                                          #\\)
                                         ((char= next-char #\")
                                          (read-char in)
                                          #\")
                                         ((char= next-char #\x)
                                          (code-char (read-hexcode in))))
                        :else
                          :collect char)
                  'string))))

(defun solve/1 (pathname)
  (with-open-file (in pathname)
    (loop :with char-count := 0
          :with escaped-char-count := 0
          :for line := (read-line in nil nil)
          :while line
          :do (let ((escaped-string (read-string line)))
                (incf char-count (length line))
                (incf escaped-char-count (length escaped-string)))
          :finally (return (values char-count escaped-char-count (- char-count escaped-char-count))))))

(defun escape-string (line)
  (with-output-to-string (out)
    (loop :for char :across line
          :do (case char
                (#\\ (write-char char out)
                     (write-char char out))
                (#\" (write-char #\\ out)
                     (write-char #\" out))
                (t (write-char char out))))))


(defun solve/2 (pathname)
  (with-open-file (in pathname)
    (loop :with orig-count := 0
          :with escaped-count := 0
          :for line := (read-line in nil nil)
          :while line
          :do (incf orig-count (length line))
              (incf escaped-count (+ 2
                                     (length  (escape-string line))))
          :finally (return (values orig-count escaped-count (- escaped-count orig-count))))))
