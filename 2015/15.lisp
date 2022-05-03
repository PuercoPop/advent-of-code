(defpackage "DAY/15"
  (:use "CL"
        "SPLIT-SEQUENCE"
        "SERAPEUM"))
(in-package "DAY/15")

(defparameter +input+ #P"15.input")
(defparameter +example+ #P"15.example")

#|

Ok, so I'm blowing the heap. Time to do some estimations about how much memory
keeping all the possible combination of mixtures in memory at once.

Using the binomial coefficient we know that there are 100!/(4!*96!)=3,921,225
different way to mix the ingredients. That is almost 4 million!

If we only store the signature, as a string, we would have 43 bytes as a median
size. We are assuming the count takes 2 characters and that every character fits
in 1 byte. The outlier were one ingredient would be 100 (3 bytes) can be
discounted for the purpose of estimation, since it would mean the other counts
are 0 (1 byte).

Sprinkles: 9
Butterscotch: 12
Chocolate: 9
Candy: 5
(+ 9 12 9 5 (* 2 4)) 43 bytes

(* 3921225 43) 168612675 Bytes which is
/1024 is ≅ 164660 KiB
/1024 is ≅ 169 MiB
So we should be able to fit everything in memory. 🤔

The reason why would be because I'm starting from an empty bag and start
branching out, keeping track of all the paths seen across the way. That would
blow up the amount of memory required. Another approach would be to to start
from one possible complete mix, and branch from there through the possible
permutations.

Because every combination we are going to look at is a complete mix we can use
the seen map to keep track of the bag-score.

|#

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

(defun mix-sig (x)
  "Return a unique signature for the bag."
  (with-output-to-string (out)
    (dolist (ingredient (sort (loop :for ing :being :the :hash-keys :of x
                                    :collect ing)
                              #'string<=))
      (format out "~A~A" ingredient (gethash ingredient x)))))

;; Ignoring calories
(defun mix-score (x data-sheet)
  (reduce #'*
          (loop :for prop :in '(capacity durability flavor texture)
                :collect (max 0
                              (loop :for ingredient :being :the :hash-key :of x
                                    :for count :being :the :hash-value :of x
                                    :sum (* count (slot-value (gethash ingredient data-sheet) prop)))))))

(defun initial-mix (data-sheet upper-bound)
  (let ((ht (make-hash-table :test #'equalp)))
    (loop :for ingredient :being :the :hash-key :of data-sheet
          :do (setf (gethash ingredient ht) 0))
    (with-hash-table-iterator (next data-sheet)
      (multiple-value-bind (entryp key value)
          (next)
        (declare (ignore value))
        (assert entryp () "Hash table appears to be empty.")
        (setf (gethash key ht) upper-bound)))
    ht))

(defun ⨯ (a b)
  (loop :for x :in a
        :nconc (loop :for y :in b :collect (list x y))) )

(defun neighboors (mix upper-bound)
  (let* ((can-remove (loop :for k :being :the :hash-keys :of mix
                          :for v :being :the :hash-values :of mix
                          :when (plusp v)
                            :collect k))
         (can-add (loop :for k :being :the :hash-keys :of mix
                       :for v :being :the :hash-values :of mix
                       :when (< v upper-bound)
                         :collect k))
         (changes (⨯ can-remove can-add)))
    (loop :for (del add) :in changes
          :for ht := (alexandria:copy-hash-table mix)
          :do (decf (gethash del ht))
              (incf (gethash add ht))
          :collect ht)))

(defun solve/1 (input target)
  (let* ((ingredients (with-open-file (in input)
                       (loop :for line := (read-line in nil)
                             :while line
                             :collect (read-ingredient line))))
         (data-sheet (loop :with ht := (make-hash-table :test #'equalp)
                           :for ingredient :in ingredients
                           :do (setf (gethash (name ingredient) ht) ingredient)
                           :finally (return ht)))
         (seen (make-hash-table :test #'equalp))
         (initial-mix (initial-mix data-sheet target))
         (mixes (queue initial-mix)))
    (setf (gethash (mix-sig initial-mix) seen)
          (mix-score initial-mix data-sheet))
    (loop :while (progn
                   ;; (format t "Queue len: ~A~%" (qlen mixes))
                   (not (queue-empty-p mixes)))
          :for current := (deq mixes)
          :for neighboors := (progn
                               (format t "N:~%")
                               (dolist (m (neighboors initial-mix target))
                                 (format t "~A~%" (mix-sig m)))
                               (format t "END:~%")
                               (neighboors initial-mix target))
          :do (loop :for m :in neighboors
                    :unless (gethash (mix-sig m) seen)
                      :do (setf (gethash (mix-sig m) seen)
                                (mix-score initial-mix data-sheet))
                          (format t "Queueing ~A~%" (mix-sig m))
                          (enq m mixes)
                    ))
    (values seen mixes)
    ;; (loop :for mix :in combinations
    ;;       :maximize (bag-score mix data-sheet))
    ))
