(defpackage "DAY/10"
  (:use "CL"
        "ALEXANDRIA"))
(in-package "DAY/10")

(defun concat (&rest strings)
  (apply 'concatenate 'string strings))

(defun times-repeated-p (input)
  "Returns three values:
   1. The number of times the digit is repeated.
   2. The digit that is repeated
   3. The rest of the sequence"
  (let ((digit-char (aref input 0))
        
        (times (loop :for index :from 0 :upto (1- (length input))
                     :until (char/= (aref input 0)
                                    (aref input index))
                     :finally (return index))))
    (values times (digit-char-p digit-char) (subseq input times))))

(times-repeated-p "11")
(times-repeated-p "111221") ; => 3, 1, "221"
;; => "3", "1", "221"

(defun string->alist (input)
  "Transform to an alist representation of the sequence."
  (labels ((iter (input alist)
             (cond ((string= "" input) (reverse alist))
                   (t (multiple-value-bind (times digit rest)
                          (times-repeated-p input)
                        (iter rest (acons times digit alist)))))))
    (iter input nil)))
(string->alist "1") ; => ((1 . 1))
(string->alist "11") ; => ((2 . 1))
(string->alist "21") ; => ((1 . 2) (1 . 1))
(string->alist "1211") ; => ((1 . 1) (1 . 2) (2 . 1))
(string->alist "111221") ; => ((3 . 1) (2 . 2) (1 . 1))
(string->alist "3122111") ; => ((1 . 3) (1 . 1) (2 . 2) (3 . 1))

(defun count-digits (alist)
  (reduce '+ alist :key 'car))
(count-digits (string->alist "111221"))

(flatten (string->alist "111221"))
(defun group-by-number (xs)
  (labels ((iter (xs last-digit count result)
             (cond ((endp xs) result)
                   ((eql last-digit (car xs)))
                   )))))


(defun next (alist)
  (loop :for cons :in alist
        :collect (cons (cdr cons) (car cons))))
(next (string->alist "1"))

(defun next (input)
  (labels ((iter (input next)
             (multiple-value-bind (times digit rest) (times-repeated-p input)
               (cond ((string= rest "") (concat next times digit))
                     (t (iter rest (concat next times digit)))))))
      (iter input "")))

(defun expand (input times)
  (loop :repeat times
        :do (setf input (next input))
        :finally (return (length input))))

(defun expand* (input times)
  (cond ((zerop times) (length input))
        (t (expand* (next input) (1- times)))))

;; (next "1")
;; (next "11") ; => "21"
;; (next "1211") ; => "111221"
;; (next "111221")
;; (expand* "1" 3)
;; (expand "1113122113" 30)
;; (expand "1113122113" 40)
;; (expand* "1113122113" 40)
