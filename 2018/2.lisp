(defpackage "AOC/2"
  (:use "CL"))
(in-package "AOC/2")

(defparameter +input+ #P"2.input")

(defun has-count (count table)
  (loop :for c :being :the :hash-value :of table
        :when (= c count)
          :do (return t)))

(defun frequency-table (word)
  (loop
    :with freq-table := (make-hash-table)
    :for char :across word
    :for count := (gethash char freq-table 0)
    :do (setf (gethash char freq-table)
              (1+ count))
    :finally (return freq-table)))

(defun analyze-word (word)
  (let ((freq-table (frequency-table word)))
    (values (has-count 2 freq-table)
            (has-count 3 freq-table))))

(defun solve-1 ()
  (with-open-file (in +input+)
    (loop
      :with 2-count := 0
      :with 3-count := 0
      :for line := (read-line in nil)
      :while line
      :do
         (multiple-value-bind (2-of-a-kind 3-of-a-kind)
             (analyze-word line)
           (when 2-of-a-kind
             (incf 2-count))
           (when 3-of-a-kind
             (incf 3-count)))
      :finally (return (values (* 2-count 3-count)
                               2-count
                               3-count)))))

(defun compare-words (word-1 word-2)
  (loop :for i :below (length word-1)
        :count (char/= (aref word-1 i)
                       (aref word-2 i))))

(defun solve-2 ()
  (let ((words (with-open-file (in +input+)
                 (coerce (loop :for word := (read-line in nil)
                               :while word
                               :collect word)
                         'vector))))
    (loop :named outer
          :for i :below (length words)
          :for word-1 := (aref words i)
          :do (loop :for j :below (length words)
                    :for word-2 := (aref words j)
                    :when (= 1 (compare-words word-1 word-2))
                      :do (return-from outer (values word-1 word-2))))))

(analyze-word "abcdef") ; => NIL, NIL
(analyze-word "bababc") ; => T, T
(analyze-word "abbcde") ; => T, NIL
(analyze-word "abcccd") ; => NIL, T
(analyze-word "aabcdd") ; => T, NIL
(analyze-word "abcdee") ; => T, NIL
(analyze-word "ababab") ; => NIL, T

(compare-words "abcde" "axcye")
(compare-words "fghij" "fguij")
