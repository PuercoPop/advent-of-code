;; Improvements
;;
;; - Replace the (setf current-password ...) forms by using
;; - collect/append and two loops.
;; - Use keywords instead of strings for keys in the alist.

(defpackage "DAY/04"
  (:use "CL"
        "SPLIT-SEQUENCE"))

(in-package "DAY/04")

(defparameter +input+ #P"4.input")
(defparameter +example+ #P"4.example")

(defun read-passports (path)
  (let ((passports (list))
        (current-passport nil))
    (with-open-file (in path)
      (loop :for line := (read-line in nil)
            :while line
            :do  (if (empty-line-p line)
                     (progn
                       (push current-passport passports)
                       (setf current-passport nil))
                     (setf current-passport (append current-passport (read-fields line))))
            :finally (push current-passport passports)))
    (apply #'vector (nreverse passports))))

(with-open-file (in #P"4.example")
  (read-fields (read-line in)))

(defun read-fields (line)
  (mapcar 'read-field (split-sequence #\Space line)))

(defun read-field (field)
  (destructuring-bind (key value)
      (split-sequence #\: field)
    (cons key value)))

(defun empty-line-p (line)
  (every #'sb-unicode:whitespace-p line))

(defparameter +required-fields+ '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(defun passport-validp (passport)
  (let ((keys (mapcar 'car passport)))
    (every #'identity
           (loop :for req-field :in +required-fields+
                 :collect (member req-field keys :test #'string=)))))

(defun check-hgt (height)
  (let* ((pivot (- (length height) 2))
         (unit (subseq height pivot))
         (quantity (parse-integer (subseq height 0 pivot) :junk-allowed t)))
    (cond ((string= "in" unit)
           (<= 59 quantity 76))
          ((string= "cm" unit)
           (<= 150 quantity 193))
          (t nil))))

(defun passport-validp* (passport)
  (let* ((keys (mapcar 'car passport))
         (every-field-presentp (every #'identity
                                      (loop :for req-field :in +required-fields+
                                            :collect (member req-field keys :test #'string=)))))
    (when every-field-presentp
      (let* ((byr (parse-integer (serapeum:assocdr "byr" passport :test #'string=)))
             (check-byr (<= 1920  byr 2002))
             (iyr (parse-integer (serapeum:assocdr "iyr" passport :test #'string=)))
             (check-iyr (<= 2010  iyr 2020))
             (eyr (parse-integer (serapeum:assocdr "eyr" passport :test #'string=)))
             (check-eyr (<= 2020 eyr 2030))
             (hgt (serapeum:assocdr "hgt" passport :test #'string=))
             (check-hgt (check-hgt hgt))
             (hcl (serapeum:assocdr "hcl" passport :test #'string=))
             (check-hcl (and (char= #\# (char hcl 0))
                             (every #'identity
                                    (loop :for char :across (subseq hcl 1)
                                          :collect (member char '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f")
                                                           :test #'string=))) ))
             (ecl (serapeum:assocdr "ecl" passport :test #'string=))
             (check-ecl (member ecl '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))
             (pid (serapeum:assocdr "pid" passport :test #'string=))
             (check-pid (= 9 (length pid))))
              (and check-byr
           check-iyr
           check-eyr
           check-hgt
           check-hcl
           check-ecl
           check-pid)))))


(defun part-1 (path)
  (let ((passports (read-passports path)))
    (loop :for passport :across passports
          :count (passport-validp passport))))

(defun part-2 (path)
  (let ((passports (read-passports path)))
    (loop :for passport :across passports
          :count (passport-validp* passport))))
