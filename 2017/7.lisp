(defpackage "AOC/7"
  (:use "CL" "SPLIT-SEQUENCE"))
(in-package "AOC/7")

(defparameter +input+ #P"7.input")

(defun %parse-rest (children)
  (mapcar (lambda (child)
            (intern
             (string-upcase
              (string-trim '(#\,) child))))
          children))

(defun parse-line (line)
  (let ((xs (split-sequence #\Space line)))
    (make-instance 'tree
                   :name (intern (string-upcase (car xs)))
                   :weight (let ((weight-string (cadr xs)))
                             (parse-integer (subseq weight-string 1 (1- (length weight-string)))))
                   :children (%parse-rest (cdddr xs)))))

(defparameter +discs+
  (with-open-file (in +input+)
    (loop
      :for line := (read-line in nil 'eof)
      :until (eq line 'eof)
      :collect (let ((disc (parse-line line)))
                 (update-index disc)
                 disc))))

(defparameter +disc-index+ (make-hash-table))

(defclass tree ()
  ((name :initarg :name :reader tree-name)
   (weight :initarg :weight :reader tree-weight)
   (children :initarg :children :initform () :accessor tree-children)
   (total-weight :initform nil :accessor tree-total-weight)))

(defmethod print-object ((object tree) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "name: ~A, total-weight: ~A, weigth: ~A children: ~(~A~^^,~).~%"
            (tree-name object)
            (tree-total-weight object)
            (tree-weight object)
            (tree-children object))))

(defun update-index (disc)
  (setf (gethash (tree-name disc) +disc-index+)
        disc))

(defun lookup-by-name (disc-name)
  (gethash disc-name +disc-index+))

(defun replace-names-with-discs (disc)
  (unless (endp (tree-children disc))
    (setf (tree-children disc)
          (mapcar #'lookup-by-name (tree-children disc)))
    (map nil 'replace-names-with-discs (tree-children disc))))

(replace-names-with-discs (lookup-by-name 'aapssr))

(defun disc-with-parent (discs)
  (let ((not-orphans (loop
      :for disc :in discs
      :appending (tree-children disc))))
    (remove-duplicates not-orphans :test 'string=)))

(defun orphan-disc (discs)
  (let ((not-orphans (loop
      :for disc :in discs
      :appending (tree-children disc))))
    (set-difference (mapcar 'tree-name +discs+)
                    (remove-duplicates not-orphans :test 'string=)
                    :test 'string=)))

(defparameter +disc-tree+ (build-tree "aapssr" +discs+))

(defparameter +root-disc+ (lookup-by-name 'aapssr))

(defun calc-weights (disc)
  (let* ((children (tree-children disc))
         (total-weight (if (endp children)
                           (tree-weight disc)
                           (apply '+ (tree-weight disc)
                                  (mapcar 'calc-weights children)))))
    (setf (tree-total-weight disc)
          total-weight)
    total-weight))

;; QWADA 61710, rest 61698
(- 61710 61698) ; => 12 (4 bits, #xC, #o14, #b1100)
;; PKOWHQ 7143, rest 7131
(- 7143 7131) ; => 12 (4 bits, #xC, #o14, #b1100)
;; FQKBSCN +6 => 424

;; Print children of zfrssm tlskukk fqkbscn mlafk


(defun row-total-weights (row-number)
  (if (zerop row-number)
      (format t "~{~A~^,~}~%" ())))
(- 40305 6)
40299
(- 1464 6)
1458
