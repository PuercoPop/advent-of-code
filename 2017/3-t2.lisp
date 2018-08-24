(defpackage "AOC/3"
  (:use "CL"))
(in-package "AOC/3")

(defparameter +input+ 325489)

(defun point-+ (point-1 point-2)
  (cons (+ (car point-1)
           (car point-2))
        (+ (cdr point-1)
           (cdr point-2))))

(defun in-bounds (point bounds)
  (and (<= (caar bounds)
           (car point)
           (cadr bounds))
       (<= (cdar bounds)
           (cdr point)
           (cddr bounds))))

(defun update-bounds (point bounds)
  (cond ((> (caar bounds) (car point))
         (setf (caar bounds) (1- (caar bounds))))
        ((> (car point) (cadr bounds))
         (setf (cadr bounds) (1+ (cadr bounds))))
        ((> (cdar bounds) (cdr point))
         (setf (cdar bounds) (1- (cdar bounds))))
        ((> (cdr point) (cddr bounds))
         (setf (cddr bounds) (1+ (cddr bounds))))
        (t (error "Illegal State")))
  bounds)

(defun build-grid (limit)
  (let ((point (cons 0 0))
        (room-number 1)
        (known-bounds (cons (cons 0 0)
                            (cons 0 0)))
        (direction '#1=(( 1 .  0)
                        ( 0 .  1)
                        (-1 .  0)
                        ( 0 . -1) . #1#)))

    (tagbody
     check-condition
       (when (= room-number limit)
         (go end))
     step
       (setf point (point-+ point (car direction))
             room-number (1+ room-number))
     check-bounds
       (when (in-bounds point known-bounds)
         (go check-condition))
     update-bounds
       (setf known-bounds (update-bounds point known-bounds)
             direction (cdr direction))
       (go check-condition)
     end)
    point))

(defun deref-cell (point grid)
  (gethash point grid 0))

(defun sum-neighboors (point grid)
  (+ (deref-cell (point-+ point '(1 . 0)) grid)
     (deref-cell (point-+ point '(1 . 1)) grid)
     (deref-cell (point-+ point '(0 . 1)) grid)

     (deref-cell (point-+ point '(-1 . 1)) grid)
     (deref-cell (point-+ point '(-1 . 0)) grid)
     (deref-cell (point-+ point '(-1 . -1)) grid)

     (deref-cell (point-+ point '(0 . -1)) grid)
     (deref-cell (point-+ point '(1 . -1)) grid)))

(defun build-grid-2 (limit)
  (let ((grid (make-hash-table :test 'equalp))
        (point (cons 0 0))
        (room-number 1)
        (known-bounds (cons (cons 0 0)
                            (cons 0 0)))
        (direction '#1=(( 1 .  0)
                        ( 0 .  1)
                        (-1 .  0)
                        ( 0 . -1) . #1#)))
    (setf (gethash point grid) room-number)
    (tagbody
     check-condition
       (when (> (gethash point grid) limit)
         (go end))
       (format t "Checking if ~A > ~A~%" (gethash point grid) limit)
     step
       (setf point (point-+ point (car direction))
             (gethash point grid) (sum-neighboors point grid))
     check-bounds
       (when (in-bounds point known-bounds)
         (go check-condition))
     update-bounds
       (setf known-bounds (update-bounds point known-bounds)
             direction (cdr direction))
       (go check-condition)
     end)
    (values (gethash point grid) point grid)))
