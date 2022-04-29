(defpackage "DAY/15"
  (:use "CL"
        "SPLIT-SEQUENCE"
        "SERAPEUM"))
(in-package "DAY/15")

(defparameter +input+ #P"15.input")
(defparameter +example+ #P"15.example")

(defclass ingredient ()
  ((name :initarg :name :reader name)
   (capacity :initarg :capacity :reader capacity)
   (durability :initarg :durability :reader durability)
   (flavor :initarg :flavor :reader flavor)
   (texture :initarg :texture :reader texture)
   (calories :initarg :calories :reader calories)))

(defmethod print-object ((obj ingredient) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (name obj))))

(defun read-ingredient (line)
  (let* ((colon (position #\: line))
         (name (subseq line 0 colon))
         (attributes (subseq line (1+ colon)))
         (parse-attribute (lambda (attr)
                            (let* ((attr (string-trim '(#\Space) attr))
                                   (space (position #\Space attr))
                                   (name (intern (string-upcase (subseq attr 0 space))
                                                 "KEYWORD"))
                                   (value (parse-integer (subseq attr space))))
                              (list name value)))))
    (apply #'make-instance 'ingredient :name name
           (loop :for attr :in (split-sequence #\, attributes)
                 :append (funcall parse-attribute attr)))))
(read-ingredient "Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3")

(defun bag-count (bag)
  (loop :for n :being :the :hash-values :of bag
        :sum n))

(defun bag-sig (x)
  "Return a unique signature for the bag."
  (with-output-to-string (out)
    (dolist (ingredient (sort (loop :for ing :being :the :hash-keys :of x
                                    :collect ing)
                              #'string<=))
      (format out "~A~A" ingredient (gethash ingredient x)))))

;; Ignoring calories
(defun bag-score (x data-sheet)
  (reduce #'*
          (loop :for prop :in '(capacity durability flavor texture)
                :collect (max 0
                              (loop :for ingredient :being :the :hash-key :of x
                                    :for count :being :the :hash-value :of x
                                    :sum (* count (slot-value (gethash ingredient data-sheet) prop)))))))

(defun unique-combinations (ingredient-names target)
  (let ((seen (make-hash-table :test #'equalp))
        (completed (queue))
        (incomplete (queue)))
     (loop :for i :across ingredient-names
           :for ht := (make-hash-table :test #'equalp)
           :do (incf (gethash i ht 0))
               (setf (gethash (bag-sig ht) seen) t)
               (enq ht incomplete))
    (loop :while (qlist incomplete)
          :for current := (deq incomplete)
          :do
             (if (= (bag-count current) target)
                 (enq current completed)
                 (let ((next (loop :for i :across ingredient-names
                                   :for ht := (alexandria:copy-hash-table current)
                                   :do (incf (gethash i ht 0))
                                   :collect ht)))
                   (loop :for n :in next
                         :when (not (gethash (bag-sig n) seen))
                           :do (enq n incomplete)
                               (setf (gethash (bag-sig n) seen) t)))))
    (qlist completed)))

(defun solve/1 (input target)
  (let* ((ingredients (with-open-file (in input)
                       (loop :for line := (read-line in nil)
                             :while line
                             :collect (read-ingredient line))))
         (data-sheet (loop :with ht := (make-hash-table :test #'equalp)
                         :for ingredient :in ingredients
                         :do (setf (gethash (name ingredient) ht) ingredient)
                         :finally (return ht)))
         (ingredient-names (map 'vector #'name ingredients))
         (combinations (unique-combinations ingredient-names target)))
    (loop :for mix :in combinations
          :maximize (bag-score mix data-sheet))))

;; (let* ((ingredients (with-open-file (in +example+)
;;                        (loop :for line := (read-line in nil)
;;                              :while line
;;                              :collect (read-ingredient line))))
;;        (data-sheet (loop :with ht := (make-hash-table :test #'equalp)
;;                          :for ingredient :in ingredients
;;                          :do (setf (gethash (name ingredient) ht) ingredient)
;;                          :finally (return ht)))
;;        (bag (make-hash-table :test #'equalp)))
;;   (setf (gethash "Butterscotch" bag) 44)
;;   (setf (gethash "Cinnamon" bag) 56)
;;   (bag-score bag data-sheet))