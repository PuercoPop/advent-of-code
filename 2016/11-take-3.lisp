(defpackage "DAY/11-3"
  (:use "CL"
        "FSET")
  (:shadowing-import-from :fset
			  ;; Shadowed type/constructor names
			  #:set #:map
			  ;; Shadowed set operations
			  #:union #:intersection #:set-difference #:complement
			  ;; Shadowed sequence operations
			  #:first #:last #:subseq #:reverse #:sort #:stable-sort
			  #:reduce
			  #:find #:find-if #:find-if-not
			  #:count #:count-if #:count-if-not
			  #:position #:position-if #:position-if-not
			  #:remove #:remove-if #:remove-if-not
			  #:substitute #:substitute-if #:substitute-if-not
			  #:some #:every #:notany #:notevery))
(in-package "DAY/11-3")


;; Queue

(defun make-queue ()
  (make-instance 'cl-heap:priority-queue))

(defun enqueue (queue item)
  (cl-heap:enqueue queue item (distance item)))

(defun dequeue (queue)
  (cl-heap:dequeue queue))

(defun empty-queue-p (queue)
  (cl-heap:empty-queue queue))


;; Rules
;; =====
;; if a chip is ever left in the same area as another RTG, and it's not
;; connected to its own RTG, the chip will be fried

;; Elevador
;; ========
;; - Puede mover 2 cosas.
;; - No puede moverse vacío
;; - Irradia todo lo que esta en el intermedio

;; Equivocado, cada item en su columna


(defparameter +sg+ #\S)
(defparameter +pg+ #\P)
(defparameter +tg+ #\T)
(defparameter +rg+ #\R)
(defparameter +cg+ #\C)

(defparameter +sc+ #\s)
(defparameter +pc+ #\p)
(defparameter +tc+ #\t)
(defparameter +rc+ #\r)
(defparameter +cc+ #\c)

(defparameter +test-building+ (make-array 4 :initial-contents (list (set #\h #\l)
                                                                    (set #\H)
                                                                    (set #\L)
                                                                    (set))))

(defparameter +initial-building+ (make-array 4 :initial-contents (list (set #\S #\s #\P #\p)
                                                                       (set #\T #\R #\r #\C #\c)
                                                                       (set #\t)
                                                                       (set))))

(defparameter +winning-building+ (make-array 4 :initial-contents (list (set)
                                                                       (set)
                                                                       (set)
                                                                       (set #\S #\s #\P #\p #\T #\t #\R #\r #\C #\c))))

(defclass state ()
  ((building :initarg :building :reader building)
   (elevator :initarg :elevator :accessor elevator)
   (distance :initarg :distance :initform 0 :reader distance)))

(defmethod print-object ((obj state) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "D: ~A, E: ~A, B: ~A" (distance obj) (elevator obj) (building obj))))

(make-instance 'state :building +initial-building+ :elevator 0 :distance 0) ; => #<STATE D: 0, E: 0, B: #(#{ P S p s } #{ C R T c r } #{ t } #{ })>

(defvar *number-of-elements*)

(defun winning-p (state)
  (= *number-of-elements* (size (aref (building state) 3))))

(defun possible-cargos (floor)
  (let ((result (set)))
    (do-set (first-el floor)
      (includef result (set first-el))
      (do-set (second-el (less floor first-el))
        (includef result (set first-el second-el))))
    result))

(defun chipp (o)
  (lower-case-p o))

(defun generatorp (o)
  (upper-case-p o))

(defun chips (floor)
  (filter #'chipp floor))

(defun chips-wo-generator (floor)
  (unless (empty? (chips floor))
    (every (lambda (c)
             (not (contains? floor
                             (char-upcase c))))
           (chips floor))))

(defun generators-present-p (floor)
  (some #'generatorp floor))

(defun chip-fried-p-1 (floor)
  (and (chips-wo-generator floor) (generators-present-p floor)))

(defun chip-fried-p (state)
  (some #'chip-fried-p-1 (building state)))

(defun adjacent-floors (state)
  (case (elevator state)
    (0 '(1))
    (1 (if (empty? (aref (building state) 0))
        '(2)
        '(0 2)))
    (2 (if (and (empty? (aref (building state) 0))
                (empty? (aref (building state) 1)))
           '(3)
           '(1 3)))
    (3 '(2))))

;; Update state?
(defun remove-cargo (cargo floor)
  (set-difference floor cargo))

(defun insert-cargo (cargo floor)
  ;; unionf?
  (union cargo floor))

(defun move (cargo destination state)
  (let ((from-floor (remove-cargo cargo (aref (building state)
                                              (elevator state))))
        (to-floor (insert-cargo cargo (aref (building state)
                                            destination)))
        (new-building (copy-seq (building state))))
    (setf (aref new-building (elevator state))
          from-floor
          (aref new-building destination)
          to-floor)
    (make-instance 'state
                   :building new-building
                   :elevator destination
                   :distance (1+ (distance state)))))

(defun move-up (cargo state)
  (assert (> 3 (elevator state)))
  (move cargo (1+ (elevator state)) state))

(defun move-down (cargo state)
  (assert (< 0 (elevator state)))
  (move cargo (1- (elevator state)) state))

(defun current-floor (state)
  (aref (building state) (elevator state)))

(defun equivalent-state (next-state past-state)
  )

(defun seenp (next-state past-states)
  (find-if (alexandria:curry 'equivalent-state next-state)
           past-states
           :key (lambda (i) (@ i 1))))

(defun next-states (state past-states)
  (let ((possible-cargos (possible-cargos (current-floor state)))
        (possible-destinations (adjacent-floors state))
        (result)
        (discarded-states))
    (loop :for elevator-destination :in possible-destinations
          :do (do-set (cargo possible-cargos)
                (let ((next-state (move cargo elevator-destination state)))
                  (if (or (contains? past-states (seq (elevator next-state)
                                                      (building next-state)))
                              (chip-fried-p next-state))
                      (push next-state discarded-states)
                      (push next-state result)))))
    (values result discarded-states)))

(defvar *console* (iolib/trivial-sockets:open-stream "127.0.0.1" 6007))

(defun solve-1 (initial-building)
  ;; Breadth-First return first hit on winning position
  (let ((current (make-instance 'state :building initial-building :elevator 0 :distance 0))
        (queue (make-queue))
        (past-states (set)))
    (setf *number-of-elements* (reduce '+ (building current) :key 'size))
    (loop
      ;; Keep distance in current
      (format *console* "Queue Size: ~A~%" (cl-heap:queue-size queue))
      (when current (format *console* "Current: ~A~%" current))
      (when current (format *console* "Distance: ~A~%" (distance current)))
      (finish-output *console*)
      (cond ((null current) (return :not-found))
            ((winning-p current) (return current))
            (t (cl:map nil
                       (alexandria:curry #'enqueue queue)
                       (next-states current past-states))
               (includef past-states (seq (elevator current)
                                             (building current)))
               (setf current (dequeue queue)))))))

;; Sin Prune 1219
;; 1er hint = 416
