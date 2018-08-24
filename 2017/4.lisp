(defpackage "AOC/4"
  (:use "CL" "SPLIT-SEQUENCE"))
(in-package "AOC/4")

(defparameter +seen+ (make-hash-table :test 'equalp))

(defun passphrase-valid-p (line)
  (loop
    :with seen := (make-hash-table :test 'equalp)
    :for word :in (split-sequence #\Space line)
    :when (gethash word seen)
      :do (return nil)
    :do
       (setf (gethash word seen) t)
    :finally (return t)))

(with-open-file (in #P"4.input")
  (loop for line := (read-line in nil 'eof)
        :while (not (eq line 'eof))
        :when (passphrase-valid-p line)
          :sum 1))

(defun word-histogram (word)
  (let ((histogram (make-hash-table :test 'equal)))
    (loop
      :for char :across word
      :do (setf (gethash char histogram) (1+ (gethash char histogram 0))))
    histogram))

(defun histogram-equal (h1 h2)
  (and (every (lambda (x)
                (eq x t))
              (loop
                :for key being the hash-keys of h1
                :collect (eq (gethash key h1)
                             (gethash key h2))))
       (every (lambda (x)
                (eq x t))
              (loop
                :for key being the hash-keys of h2
                :collect (eq (gethash key h1)
                             (gethash key h2))))))

(defun passphrase-no-anagrams-p (line)
  (let ((word-histograms (loop
                           :for word :in (split-sequence #\Space line)
                           :collect (word-histogram word))))
    (loop
      :for start :from 1 :below (length word-histograms)
      :for current := (elt  word-histograms (1- start))
      :for view := (subseq word-histograms start)
      :do (loop
            :for histogram :in view
            :when (histogram-equal current histogram)
              :do (return-from passphrase-no-anagrams-p nil)))
    t))

(passphrase-valid-p "abcde xyz ecdab")
(passphrase-valid-p "iiii oiii ooii oooi oooo")

(with-open-file (in #P"4.input")
  (loop for line := (read-line in nil 'eof)
        :while (not (eq line 'eof))
        :when (passphrase-no-anagrams-p line)
          :sum 1))

208
