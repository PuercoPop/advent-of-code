(defpackage "DAY/2"
  (:use "CL"
        "SPLIT-SEQUENCE"))

(in-package "DAY/2")

(defparameter +input+ #P"2.input")

(with-open-file (in +input+)
  (defparameter +program-1+ (coerce (mapcar 'parse-integer (split-sequence #\, (read-line in)))
                                    'vector)))

(defun run (program)
  (loop
    :for pc := 0 :then (+ 4 pc)
    :for opcode := (aref program pc)
    :while (/= opcode 99)
    :do (let* ((op (ecase opcode
                     (1 #'+)
                     (2 #'*)))
               (op-1-ix (aref program (+ 1 pc)))
               (op-2-ix (aref program (+ 2 pc)))
               (ret-ix  (aref program (+ 3 pc)))
               (op-1 (aref program op-1-ix))
               (op-2 (aref program op-2-ix)))
          (setf (aref program ret-ix)
                (funcall op op-1 op-2)))
    :finally (return program)))

(defun solve/1 ()
  (with-open-file (in +input+)
    (let ((program (coerce (mapcar 'parse-integer (split-sequence #\, (read-line in)))
                           'vector)))
      (setf (aref program 1) 12)
      (setf (aref program 2) 2)
      (run program))))

(defun setup-program (program noun verb)
  (let ((program (copy-seq program)))
    (setf (aref program 1) noun)
    (setf (aref program 2) verb)
    program))

(defun noun-verb-pairs ()
  (loop :for noun :upto 99
        :appending (loop :for verb :upto 99
                  :collect (cons noun verb))))

(defun solve/2 ()
  (with-open-file (in +input+)
    (let ((program (coerce (mapcar 'parse-integer (split-sequence #\, (read-line in)))
                           'vector)))
      (loop :for (noun . verb) :in (noun-verb-pairs)
            :for prog := (setup-program program noun verb)
            :until (= (aref (run prog)
                            0)
                      19690720)
            :finally (return (values noun verb))))))


(defun test/2 ()
  (with-open-file (in +input+)
    (let ((program (coerce (mapcar 'parse-integer (split-sequence #\, (read-line in)))
                           'vector)))
      (setf (aref program 1) 25)
      (setf (aref program 2) 5)
      (run program))))
