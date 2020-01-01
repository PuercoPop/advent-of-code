(defpackage "AOC/1"
  (:use "CL"))
(in-package "AOC/1")

(defparameter +input+ #P"1.input")

(defun solve/1 (in)
  (loop
    :with count := 0
    :for char := (read-char in nil nil)
    :while char
    :do
       (cond ((char= char #\() (incf count))
             ((char= char #\)) (decf count))
             (t (warn "Expected #\(, got ~A (~A)" char (char-code char))))
    :finally (return count)))

(with-input-from-string (in "(()(()(")
  (solve/1 in))

(with-open-file (in +input+)
 (solve/1 in)) ; => 74 (7 bits, #x4A, #o112, #b1001010)

(defun solve/2 (in)
  (loop
    :with count := 0
    :with position := 1
    :for char := (read-char in nil nil)
    :while char
    :do
       (cond ((char= char #\() (incf count))
             ((char= char #\)) (decf count))
             (t (error "Expected #\(, got ~A" char)))
       (when (minusp count)
         (return position))
       (incf position)))

(with-open-file (in +input+)
 (solve/2 in)) ; => 1795 (11 bits, #x703)
