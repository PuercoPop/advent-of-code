(defpackage "AOC/6"
  (:use "CL"
        "SPLIT-SEQUENCE"
        "CONSPACK"))
(in-package "AOC/6")

(defparameter +input-file+ #P"6.input")

(defparameter +banks+
  (let ((banks (make-array 16)))
    (with-open-file (in +input-file+)
      (loop
        :for string :in (split-sequence:split-sequence #\Tab (read-line in))
        :for idx :from 0
        :for memory-blocks := (parse-integer string)
        :do (setf (aref banks idx) memory-blocks)))
    banks))
(defparameter +banks+ #(0 2 7 0))

(defparameter +bank-history+ (make-hash-table :test 'equal))

(defun bank-seen-p (bank)
  (gethash (encode bank)
           +bank-history+
           nil))

(defun step-bank (bank)
  (let ((next-bank (copy-seq bank)))
    (setf (gethash (encode bank) +bank-history+) t)
    (multiple-value-bind
          (idx number-of-blocks) (find-active-bank bank)
      (setf (aref next-bank idx) 0)
      (loop
        :until (zerop number-of-blocks)
        :do (setf idx (mod (1+ idx)
                             (length next-bank)))
            (incf (aref next-bank idx))
            (decf number-of-blocks)))
    (values next-bank (bank-seen-p next-bank))))

(defun find-active-bank (bank)
  (loop :with active-bank := 0
        :with max-memory-blocks := 0
        :for idx :from 0 :below (length bank)
        :when (> (aref bank idx) max-memory-blocks)
          :do (setf active-bank idx
                    max-memory-blocks (aref bank idx))
        :finally (return (values active-bank max-memory-blocks))))

(defun solve-1 (bank)
  (setf +bank-history+ (make-hash-table :test 'equalp))
  (loop
    ;; :for count :from 0 :upto 10
    :until (bank-seen-p bank)
    :do (setf bank (step-bank bank))
        ;; (format t "~A : ~A~%" bank (encode bank))
    :sum 1))

(defun solve-2 (bank)
  (setf +bank-history+ (make-hash-table :test 'equalp))
  (loop
    :until (bank-seen-p bank)
    :do (setf bank (step-bank bank)))
  (setf +bank-history+ (make-hash-table :test 'equalp))
  (loop
    :until (bank-seen-p bank)
    :do (setf bank (step-bank bank))
    :sum 1))

(defun trace-)


(let* ((arr (make-array 4 :initial-contents '(0 2 7 0)))
       (code-1 (prog1 (sxhash arr)
                 (setf (aref arr 2) 0)))
       (code-2 (sxhash arr)))
  (values code-1 code-2 (equalp code-1 code-2)))

(let* ((arr (make-array 4 :initial-contents '(0 2 7 0)))
       (code-1 (prog1 (conspack:encode arr)
                 (setf (aref arr 2) 0)))
       (code-2 (conspack:encode arr)))
  (values code-1 code-2 (equalp code-1 code-2) arr))
