(defpackage "DAY/10"
  (:use "CL"
        "SPLIT-SEQUENCE"
        "STRING-CASE"))
(in-package "DAY/10")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/10.input")

(defparameter +bot-rules+ (make-hash-table))
(defparameter +bot-values+ (make-hash-table))
(defparameter +output-bins+ (make-array 21 :initial-element nil))

;; value 7 goes to bot 77
;; 7 -> '(bot 77)

(defun parse-place (string)
  (destructuring-bind (target id) (split-sequence #\Space string)
    (cons (intern (string-upcase target)
                  )
          (parse-integer id))))
(parse-place "bot 77")

(defun parse-value-rule (line)
  (let* ((first-space (position #\Space line))
         (second-space (position #\Space line :start (1+ first-space)))
         (value (parse-integer (subseq line (1+ first-space) second-space)))
         (goes-to-pos (search "goes to " line))
         (goes-to-pos-length (length "goes to "))
         (bot-id (cdr (parse-place (subseq line (+ goes-to-pos-length goes-to-pos))))))

    (setf (gethash bot-id +bot-values+) (cons value (gethash bot-id +bot-values+)))
    (assert (> 3 (length (gethash bot-id +bot-values+))) nil "Bot has more than two micro-chips")))
(parse-value-rule "value 7 goes to bot 77") ; => "bot 77"



;; bot 75 gives low to bot 145 and high to bot 95
;; 75 -> '( (bot 145) . (bot 95))


;; Number of the bot that compare 61 and 17


(defun parse-bot-rule (line)
  (let* ((first-space (position #\Space line))
         (second-space (position #\Space line :start (1+ first-space)))
         (bot-id (parse-integer (subseq line (1+ first-space) second-space)))
         (low-to-pos (search "gives low to " line))
         (low-to-length (length "gives low to "))
         (and-high-to-pos (search " and high to " line))
         (and-high-to-length (length " and high to "))
         (low-rule (parse-place (subseq line (+ low-to-length low-to-pos) and-high-to-pos)))
         (high-rule (parse-place (subseq line (+ and-high-to-length and-high-to-pos)))))
    
    (setf (gethash bot-id +bot-rules+)
          (list low-rule high-rule))))
(length "gives low to ") ; => 13 (4 bits, #xD, #o15, #b1101)
(parse-bot-rule "bot 75 gives low to bot 145 and high to bot 95")

(defun parse-rule (line)
  (ecase (aref line 0)
    (#\b (parse-bot-rule line))
    (#\v (parse-value-rule line))))

(defun load-rules ()
  (with-open-file (in +input+)
    (loop :for line := (read-line in nil 'eof)
          :until (eq line 'eof)
          :do (parse-rule line))))

(defun find-active-bot ()
  (let (bot-id)
    (maphash (lambda (key values)
               (when (= 2 (length values))
                 (setf bot-id key)))  +bot-values+)
    bot-id))
(find-active-bot)

(defun resolve-rule (place value)
  (ecase (car place)
    (output (setf (aref +output-bins+ (cdr place)) value ))
    (bot (setf (gethash (cdr place) +bot-values+)
               (cons value (gethash (cdr place) +bot-values+))))))

(defun resolve-bot (bot-id)
  (let* ((values (gethash bot-id +bot-values+))
         (rule (gethash bot-id +bot-rules+))
         (low (apply 'min values))
         (high (apply 'max values)))
    (let ((low-rule (first rule))
          (high-rule (second rule)))
      
      (resolve-rule low-rule low)
      (resolve-rule high-rule high)
      (setf (gethash bot-id +bot-values+)
            nil))))

(defun next-step ()
  (let ((active-bot (find-active-bot)))
    (resolve-bot active-bot)))

(defun run ()
  (let ((active-bot (find-active-bot)))
    (when active-bot
      (let ((active-bot-values (gethash active-bot +bot-values+)))
        (when (and (member 61 active-bot-values)
                   (member 17 active-bot-values))
          (format t "First Puzzle: ~A~%" active-bot)))
      (resolve-bot active-bot)
      (run))))
