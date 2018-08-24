(in-package #:cl-user)

(defparameter *input-file* #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/5.input")

(defun nice-string-p (string)
  (let ((a-count 0)
        (e-count 0)
        (i-count 0)
        (o-count 0)
        (u-count 0)
        (succesive-letter nil)
        (forbidden-strings nil))
    (loop :with last-char := nil
          :for char :across string
          :do
             (when (char= #\a char)
               (incf a-count))
             (when (char= #\e char)
               (incf e-count))
             (when (char= #\i char)
               (incf i-count))
             (when (char= #\o char)
               (incf o-count))
             (when (char= #\u char)
               (incf u-count))

             (when (equalp last-char char)
               (setf succesive-letter t))

             (when (or (and (equalp #\a last-char)
                            (equalp #\b char))
                       (and (equalp #\c last-char)
                            (equalp #\d char))
                       (and (equalp #\p last-char)
                            (equalp #\q char))
                       (and (equalp #\x last-char)
                            (equalp #\y char)))
               (setf forbidden-strings t))

             (setf last-char char))
    (and (<= 3 (+ a-count
                  e-count
                  i-count
                  o-count
                  u-count))
         succesive-letter
         (not forbidden-strings))))

(with-open-file (in *input-file*)
  (count t
         (loop :for line := (read-line in nil 'eof)
               :until (eq line 'eof)
               :collect (nice-string-p line))))

(nice-string-p "ugknbfddgicrmopn") ;; t
(nice-string-p "aaa") ;; t
(nice-string-p "jchzalrnumimnmhp") ;; nil
(nice-string-p "haegwjzuvuyypxyu") ;; nil
(nice-string-p "dvszwmarrgswjxmb") ;; t - nil


(defun nice-string-p (string)
  (let ((condition-1 nil)
        (condition-2 nil))
    (loop :with last-char := nil
          :with ignore-last-char := 0
          :with char-2 := nil
          :with repeated-pairs := 0
          :for char :across string
          :do
             (when (and (zerop ignore-last-char)
                        (equalp last-char char))
               (incf repeated-pairs)
               (setf ignore-last-char 2))
             
             (and char char-2
                  (equalp char char-2)
                  (setf condition-2 t))

             (psetf char-2 last-char
                    last-char char)
             (unless (zerop ignore-last-char)
               (decf ignore-last-char))
             
          :finally (when (>= repeated-pairs 2)
                     (setf condition-1 t)))
    (format t "Condition 1: ~A~%Condition 2: ~A~%" condition-1 condition-2)
    (and condition-1 condition-2)))

(nice-string-p "qjhvhtzxzqqjkmpb")
(nice-string-p "")
(nice-string-p "")
(nice-string-p "")
(nice-string-p "")
