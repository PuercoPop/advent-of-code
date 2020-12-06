(defpackage "DAY/01"
  (:use "CL"))

(in-package "DAY/01")

(defparameter +input+ #P"1.input")

(defun part-1 (entries)
  (loop :for a :in entries
        :do (loop :for b :in entries
                  :do (when (and (/= a b)
                                 (= 2020 (+ a b)))
                        (return-from part-1 (values (* a b) a b))))))

(let ((numbers (with-open-file (in +input+)
                  (loop :for num := (read in nil)
                        :while num
                        :collect num))))
  (part-1 numbers))


(defun part-2 (entries)
  (loop :for a :in entries
        :do (loop :for b :in entries
                  :do (loop :for c :in entries
                            :do (when (and (/= a b c)
                                           (= 2020 (+ a b c)))
                        (return-from part-2 (values (* a b c) a b c)))))))

(let ((numbers (with-open-file (in +input+)
                  (loop :for num := (read in nil)
                        :while num
                        :collect num))))
  (part-2 numbers))
