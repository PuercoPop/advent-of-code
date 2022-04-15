(defpackage "DAY/10"
  (:use "CL"))
(in-package "DAY/10")

(defun look (in)
  "Transform the input into a sequence of (digit . count). INPUT is a
string of digits."
  (let* ((current-digit (char in 0))
         (current-pair (cons 1 current-digit)))
    (append (loop :with len := (length in)
                   :for char :across (subseq in 1)
                   :when (char/= current-digit char)
                     :collect (prog1 current-pair
                                (setf current-digit char
                                      current-pair (cons 0 char)))
                   :when (char= current-digit char)
                     :do (incf (car current-pair)))
            (list current-pair))))

(defun say (seq)
  "Transforms a sequence of pairs with the form (digit . count) to a string of digits"
  (with-output-to-string (out)
    (loop :for (count . digit) :in seq
          :do (format out "~A~A" count digit))))

(defun solve/1 (input times)
  (let ((in input))
    (loop :repeat times
          :for seq := (look in)
          :do ;; (format t "• ~A : ~A~%" in seq)
              (setf in (say seq)))
    (values (length in) in)))

(solve/1 "1113122113" 40) ; => 360154, "31131122211311123113321112131221123113111231121113311211131221121321131211132221123113112211121312211231131122211211133112111311222112111312211312111322211213211321322123211211131211121332211231131122...[sly-elided string of length 360154]"

(solve/1 "1113122113" 50) ; => 5103798, "13211321322113311213212312311211131122211213211331121321123123211231131122211211131221131112311332211213211321223112111311222112132113213221123123211231132132211231131122211311123113322112111312211312...[sly-elided string of length 5103798]"
