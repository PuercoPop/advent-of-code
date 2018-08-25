(defpackage "DAY/7"
  (:use "CL"
        "SPLIT-SEQUENCE"))
(in-package "DAY/7")

(defparameter +input+ #P"7.input")

(defun ~ (n)
  (let* ((length (integer-length n))
         (negation (mask-field (byte length 0)
                               (lognot n))))
    (if (zerop negation)
        (dpb 1 (byte 1 length) 0)
        negation)))

(defun keywordicate (string)
  (sb-int:keywordicate (string-upcase
                        (string-trim '(#\Space) string))))

(defun integer-or-symbol (x)
  (handler-case (values (parse-integer x) :input)
        (sb-int:simple-parse-error () (values (keywordicate x) :cable))))

(defun rule-type (lhs)
  (ecase (length lhs)
    (1 :input)
    (2 :unary)
    (3 :binary)))

(defgeneric make-rule (type lhs out)
  (:documentation "Return"))

(defmethod make-rule ((type (eql :input)) lhs out)
  (multiple-value-bind (in type)
      (integer-or-symbol (car lhs))
      (list :type type
            :in in
            :out out)))

(defmethod make-rule ((type (eql :unary)) lhs out)
  (list :type type
        :in (keywordicate (second lhs))
        :op (keywordicate (first lhs))
        :out out))

(defmethod make-rule ((type (eql :binary)) lhs out)
  (let* ((lhs (mapcar #'integer-or-symbol lhs))
         (op (elt lhs 1))
         (in-1 (elt lhs 0))
         (in-2 (elt lhs 2)))
    (list :type type
          :op op
          :in-1 in-1
          :in-2 in-2
          :out out)))

(defun parse-line (line)
  (let* ((->-index (1- (position #\> line :from-end t)))
         (out (keywordicate (subseq line (+ 2 ->-index))))
         (lhs (string-trim '(#\Space) (subseq line 0 ->-index)))
         (lhs (split-sequence #\Space lhs)))
    (make-rule (rule-type lhs) lhs out)))

;; (parse-line "iu RSHIFT 1 -> jn")
;; (parse-line "NOT kt -> ku") ; => (:TYPE :UNARY :IN :KT :OP :NOT :OUT :KU)

(defgeneric compute (var-or-val))

(defmethod compute ((x integer))
  x)

(defun expand-body (rule)
  (ecase (getf rule :type)
    (:cable `(compute ,(getf rule :in)))
    (:input (getf rule :in))
    (:unary `(~ (compute ,(getf rule :in))))
    (:binary (ecase (getf rule :op)
               (:rshift `(ash (compute ,(getf rule :in-1))
                              (* -1 (compute ,(getf rule :in-2)))))
               (:lshift `(ash (compute ,(getf rule :in-1))
                              (compute ,(getf rule :in-2))))
               (:and `(logand (compute ,(getf rule :in-1))
                              (compute ,(getf rule :in-2))))
               (:or `(logior (compute ,(getf rule :in-1))
                              (compute ,(getf rule :in-2))))))))

(defun expand-to-method (rule)
  (let ((out (getf rule :out))
        (method-body (expand-body rule)))
    `(defmethod compute ((x (eql ,out)))
       ,method-body)))

(defparameter +memo+ (make-hash-table))

(defmethod compute :around (x)
  (let ((cache (gethash x +memo+)))
    (if cache
        cache
        (let ((result (call-next-method)))
          (setf (gethash x +memo+)
                result)
          result))))

#+nil
(macrolet ((define-methods ()
             (with-open-file (in +input+)
               `(progn
                  ,@(loop
                      :for line := (read-line in nil)
                      :while line
                      :collect (expand-to-method (parse-line line)))))))
  (define-methods))

#+puzzle-1
(compute :a) ;; => 16076 (14 bits, #x3ECC)

#+for-puzzle-2
(defmethod compute ((x (eql :b)))
  16076)

#+puzzle-2
(compute :a) ;; => 2797 (12 bits, #xAED)

