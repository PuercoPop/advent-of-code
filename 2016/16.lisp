(defpackage "DAY/16"
  (:use "CL"))
(in-package "DAY/16")

(defparameter +test+ "110010110100")

(defparameter +input+ "10010000000110000")

(defun next (a)
  (let ((b (nreverse (copy-seq a)))
        (ret (make-string (1+ (* 2 (length a))))))
    
    (replace ret a)
    (setf (aref ret (length a))
          #\0)
    (loop
      :for char :across b
      :for i :from (1+ (length a))
      :for next-char := (if (char= char #\0)
                            #\1
                            #\0)
      :do (progn
            (setf (aref ret i)
                  next-char)))
    

    ret))

(defun checksum-1 (state)
  (coerce
   (loop
     :for i :from 0 :upto (1- (length state)) :by 2
     :collect (if (char= (aref state i)
                         (aref state (1+ i)))
                  #\1
                  #\0))
   'string))

(defun checksum (state)
  (loop
    :for next := (checksum-1 state) :then (checksum-1 next)
    :until (oddp (length next))
    :finally (return next)))

(assert (string= (checksum "110010110100")
                 "100"))

(defun fill-buffer (initial-state target-length)
  (let ((ret (loop
               :for data := initial-state :then (next data)
               :until (>= (length data) target-length)
               :finally (return data))))
    (subseq ret 0 target-length)))

(defun solve (initial-state target-length)
  (checksum (fill-buffer initial-state target-length)))

#+example
(solve "10000" 20)

#+puzzle-1
(solve 272)

#+puzzle-2
(solve +input+ 35651584)


;; (defparameter +test+ #b110010110100)

;; (defparameter +input+ #b10010000000110000)

;; Integer based implementation

;; (defun next (int)
;;   (let ((b 0)
;;         (size (1- (integer-length int))))
;;     (loop :for i :from 0 :upto size 
;;           :do (setf (ldb (byte 1 (- size i)) b) 
;;                     (if (logbitp i int)
;;                         0
;;                         1)))
;;     (logior (ash int (+ 1 size 1))
;;             b))) 

;; (assert (= (next #b10000) #b10000011110))

;; (defun checksum-1 (input &optional (number-of-bits (integer-length input)))
;;   (let ((result 0))
;;     (loop :for j :from 0 :upto (1- (/ number-of-bits 2))
;;           :do (setf (ldb (byte 1 j) result)
;;                     (if (member (ldb (byte 2 (* 2 j)) input) '(0 3) :test '=)
;;                         1
;;                         0))
;;           :finally (return (values result (/ number-of-bits 2))))))

;; (= (checksum-1 #b110010110100) #b110101)

;; (defun checksum (int)
;;   (loop :for (result bit-length) := (multiple-value-list (checksum-1 int))
;;           :then (multiple-value-list (checksum-1 result bit-length))
;;         :until (oddp bit-length)
;;         :finally (return (values result bit-length))))

;; (assert (= (checksum #b110010110100)  #b100))

;; (defun fill-buffer (size input)
;;   (loop :for output := (next input) :then (next output)
;;         :until (< size (integer-length output))
;;         :finally (return (ldb (byte size (- (integer-length output) size)) output))))

;; (= (fill-buffer 20 #b10000) #b10000011110010000111)

;; #b10000011110010000111 ;; 20 bits
;; (checksum-1 #b10000011110010000111) ; => 501, 10
;; ;; => 501 (9 bits, #x1F5, #b111110101) (se come el 0)

;; (= (checksum (fill-buffer 20 #b10000)) #b01100)
;; (assert (= (checksum (fill-buffer 20 #b10000)) #b01100))


;; ;; Use a stream based approach to for the first checksum

;; (defun stream-fill-and-checksum (size input)
;;   )

;; ;(stream-fill-and-checksum 35651584 "10010000000110000")
