(in-package #:cl-user)

(import 'com.informatimago.common-lisp.data-encoding.hexadecimal:bytes-to-hexadecimal-string)

(defun targetp (code number)
  (string= "00000"
           (subseq (bytes-to-hexadecimal-string
                    (md5:md5sum-string (format nil "~A~A" code number)))
                   0 5)))

(loop :for candidate :from 1
      :when (targetp "bgvyzdsv" candidate)
        :do (return candidate))

(defun targetp (code number)
  (string= "000000"
           (subseq (bytes-to-hexadecimal-string
                    (md5:md5sum-string (format nil "~A~A" code number)))
                   0 6)))

(loop :for candidate :from 1
      :when (targetp "bgvyzdsv" candidate)
        :do (return candidate))
