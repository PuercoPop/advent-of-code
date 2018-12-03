(defpackage "AOC/3"
  (:use "CL"
        "SPLIT-SEQUENCE"))
(in-package "AOC/3")

(defparameter +input+ #P"3.input")

(defun read-delimited-pair (delimiter pair)
  (mapcar #'read-from-string (split-sequence delimiter pair)))

(defun parse-line (line)
  (let* ((@-pos (position #\@ line))
         (claim-id (read-from-string (subseq line 1 @-pos)))
         (nline (subseq line (1+ @-pos))))
    (destructuring-bind (pos-pair size-pair)
        (split-sequence #\: nline)
      (destructuring-bind (left-edge top-edge)
          (read-delimited-pair #\, pos-pair)
        (destructuring-bind (width height)
            (read-delimited-pair #\x size-pair)
          (values claim-id left-edge top-edge width height))))))

(parse-line "#7 @ 144,524: 23x29")

(defclass claim ()
  ((id :initarg :id :reader id)
   (left :initarg :left :reader left)
   (top :initarg :top :reader top)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(defun make-claim (id left top width height)
  (make-instance 'claim :id id :left left :top top :width width :height height))

(defparameter +claims+
  (with-open-file (in +input+)
    (loop
      :for line := (read-line in nil)
      :while line
      :collect (apply #'make-claim (multiple-value-list (parse-line line))))))

(defparameter +test-claims+
  (with-open-file (in #P"3.test")
    (loop
      :for line := (read-line in nil)
      :while line
      :collect (apply #'make-claim (multiple-value-list (parse-line line))))))


(defun solve-1 (claims)
  (let ((map (make-hash-table)))
      (flet ((note-claim (claim)
               (loop
                 :for x :from (left claim) :below (+ (left claim)
                                                    (width claim))
                 :do (loop
                       :for y :from (top claim) :below (+ (top claim)
                                                         (height claim))
                       :do (incf (gethash (complex x y) map 0))))))
        
        (map nil #'note-claim claims)
        (let ((count 0))
          (maphash (lambda (k v)
                     (declare (ignore k))
                     (when (/= 1 v)
                       (incf count)))
                   map)
          (values count map)))))

(defun draw-map (map)
  (loop :for x :upto 8
        :do (loop :for y :upto 10
                  :for count := (gethash (complex x y) map)
                  :do (if (eql count nil)
                          (princ #\.)
                          (princ count)))
            (princ #\Newline)))

(loop :for claim :in +claims+
      :maximizing (+ (left claim)
                     (width claim)))

;; 999

(loop :for claim :in +claims+
      :maximizing (+ (top claim)
                     (height claim)))

;; 1000

(defun unique-claim (map)
  (let ((claim-ids (loop :for claim :in +claims+
                         :collect (id claim)))
        (dup-ids ()))
    (loop :for x :upto 1000
          :do (loop :for y :upto 1000
                    :for coord := (complex x y)
                    :for claims := (gethash coord map ())
                    :when (and claims
                               (> (length claims) 1))
                      :do (setf dup-ids
                                (remove-duplicates (append dup-ids claims)))))
    (set-difference claim-ids dup-ids)))

(defun solve-2 (claims)
  (let ((map (make-hash-table :test 'equalp)))
      (flet ((note-claim (claim)
               (loop
                 :for x :from (left claim) :below (+ (left claim)
                                                    (width claim))
                 :do (loop
                       :for y :from (top claim) :below (+ (top claim)
                                                          (height claim))
                       :for coord := (complex x y)
                       :for current-claims := (gethash coord map ())
                       :do (setf (gethash coord map)
                                 (cons (id claim) current-claims))))))
        
        (map nil #'note-claim claims)
        map)))
