(defpackage #:day/16
  (:use #:cl
        #:split-sequence))
(in-package #:day/16)

(defparameter +input+ #P"16.input")

(defun name-keyword (name)
  (sb-int:keywordicate (string-upcase (string-trim '(#\Space) name))))

(defun parse-line (line)
  (let* ((first-colon (position #\: line))
         (header (subseq line 0 first-colon))
         (id-start (position #\Space header :from-end t))
         (sue-id (parse-integer (subseq header id-start)))
         (objects (split-sequence #\, (subseq line (1+ first-colon))))
         (objects (mapcan (lambda (object)
                            (destructuring-bind (name count)
                                (split-sequence #\: object)
                              (list (name-keyword name)
                                    (parse-integer count))))
                          objects)))
    `(:id ,sue-id ,@objects)))

(defparameter +sues+
  (with-open-file (in +input+)
    (loop :for line := (read-line in nil)
          :while line
          :collect (parse-line line))))

(defparameter +correct-sue+
  '(:children 3
    :cats 7
    :samoyeds 2
    :pomeranians 3
    :akitas 0
    :vizslas 0
    :goldfish 5
    :trees 3
    :cars 2
    :perfumes 1))

(defun match (candidate target)
  (let ((match t))
    (sb-int:doplist (obj count) candidate
      (unless (or (eq :id obj)
                  (= count (getf target obj)))
        (setf match nil)))
    match))

(defun solve-1 ()
  (loop :for sue :in +sues+
        :when (match sue +correct-sue+)
          :collect sue))

(defparameter +ops+
  (list :children #'=
        :cats #'>
        :samoyeds #'=
        :pomeranians #'<
        :akitas #'=
        :vizslas #'=
        :goldfish #'<
        :trees #'>
        :cars #'=
        :perfumes #'=))

(defun match* (candidate target)
  (let ((match t))
    (sb-int:doplist (obj count) candidate
      (unless (or (eq :id obj)
                  (funcall (getf +ops+ obj) count (getf target obj)))
        (setf match nil)))
    match))

(defun solve-2 ()
  (loop :for sue :in +sues+
        :when (match* sue +correct-sue+)
          :collect sue))
