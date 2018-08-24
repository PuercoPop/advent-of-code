(defpackage "DAY/9-2-2"
  (:use "CL"))
(in-package "DAY/9-2-2")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/9.input")

(defun end-of-file-p (in)
  (eq 'eof (peek-char nil in nil 'eof)))

(defun marker-start-p (in)
    (char-equal (peek-char nil in) #\())

(defun marker-end-p (in)
    (char-equal (peek-char nil in) #\)))

(defun read-number-until (in predicate)
  (let ((accum 0))
    (loop :for char := (read-char in nil 'eof)
          :until (funcall predicate char)
          :do (setf accum (+ (* 10 accum) (digit-char-p char))))
    accum))

(defun read-marker (in)
  (let (number-of-chars times) 
    (assert (prog1 (marker-start-p in)
              (read-char in)))
    (setf number-of-chars (read-number-until in
                                             (lambda (c) (char-equal c #\x))))
    (setf times (read-number-until in
                                   (lambda (c) (char-equal c #\)))))
    (cons number-of-chars times)))

(defun %count-characters (in markers count)
  (cond ((end-of-file-p in) count)
        ((marker-start-p in) (%count-characters in
                                                (cons (read-marker in) markers)
                                                count))
        ((not (null markers)) (destructuring-bind
                                  (width . times) (car markers)
                                  (loop :repeat times
                                        :do (read-char in))
                                  (%count-characters in (cdr markers) (+ (* width times) count))))
        ;; Default
        ((read-char in) (%count-characters in markers (1+ count)))))

(defun count-characters (in)
  (%count-characters in nil 0))


(with-open-file (in +input+)
  (count-characters in))

(with-output-to-string (out)
  (with-input-from-string (in "X(8x2)(3x3)ABCY")
    (assert (eql (length "XABCABCABCABCABCABCY") (count-characters in)))))

(with-input-from-string (in "X(8x2)(3x3)ABCY")
  (count-characters in))

(with-input-from-string (in "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")
  (assert (eql 455 (count-characters in))))


(length "XABCABCABCABCABCABCY") ; => 20 (5 bits, #x14, #o24, #b10100)
