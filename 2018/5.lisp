(defpackage "AOC/5"
  (:use "CL"))

(in-package "AOC/5")

(defparameter +input+ #P"5.input")
(defparameter +test+ #P"5.test")


(defparameter +polymer+
  (with-open-file (in +input+)
    (read-line in)))

(defparameter +test-polymer+
  (with-open-file (in +test+)
    (read-line in)))

(defun reactp (char-1 char-2)
  (and (char-equal char-1 char-2)
       (= 1 (count t (mapcar #'sb-unicode:uppercase-p
                             (list char-1 char-2))))))

(defun reduce-polymer-1 (polymer)
  (let ((ix 0)
        (ret ()))
    (tagbody
     next
       (when (>= ix (1- (length polymer)))
         (go exit))

       (if (reactp (char polymer ix)
                   (char polymer (1+ ix)))
           (go step-2)
           (if (= ix (- (length polymer) 2))
               (progn
                 (push (char polymer ix) ret)
                 (push (char polymer (1+ ix)) ret))
               (push (char polymer ix) ret)))
       
     step-1 ;; Move this to an incr?
       (setf ix (1+ ix))
       (go next)
     step-2
       (setf ix (+ 2 ix))
       (go next)
     exit)
    (coerce (nreverse ret)
            'string)))

(defun reduce-polymer (polymer)
  (loop
    :with reactedp := t
    :while reactedp
    :do (let ((reduced-polymer (reduce-polymer-1 polymer)))
          (setf reactedp (/= (length reduced-polymer)
                             (length polymer))
                polymer reduced-polymer))
    :finally (return (values polymer (length polymer)))))

(reduce-polymer-1 +test-polymer+)
(reduce-polymer-1 (reduce-polymer-1 +test-polymer+)) ; => 

(reduce-polymer-1 "dabAaCBAaD")

"dabCBAca"
"dabCBAc"

;; => 10886

;; Part 2

(defparameter +alphabet+ "abcdefghijklmnopqrstuvwyxz")

(loop :for letter :across +alphabet+
      :for length := (nth-value 1
                                (reduce-polymer (remove-if (lambda (char)
                                                             (char-equal char letter))
                                                           +polymer+)))
      :minimizing length)
