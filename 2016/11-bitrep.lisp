(defpackage "DAY/11"
  (:use "CL"
        "ALEXANDRIA"))
(in-package "DAY/11")

;; - 10 bits per floor.
;; - One item in each bit.
;; - T The generator and its corresponding microchip are 5 bits apart.
;; - Elevator counts as another floor

;; SG PG TG RG CG SC PC TC RC CC

;; #b0100001000 => PG and PC no más

(defparameter +sg+ #b1000000000)
(defparameter +pg+ #b0100000000)
(defparameter +tg+ #b0010000000)
(defparameter +rg+ #b0001000000)
(defparameter +cg+ #b0000100000)

(defparameter +sc+ #b0000010000)
(defparameter +pc+ #b0000001000)
(defparameter +tc+ #b0000000100)
(defparameter +rc+ #b0000000010)
(defparameter +cc+ #b0000000001)

(defparameter +initial-position+ (logior (ash +tc+ 20)
                                         (ash (logior +tg+ +rg+ +rc+ +cg+ +cc+) 10)
                                         (logior +sg+ +sc+ +pg+ +pc+)))

(defparameter +winning-position+ (ash (logior +sg+ +pg+ +tg+ +rg+ +cg+ +sc+ +pc+ +tc+ +rc+ +cc+)
                                      30))

(defparameter +elevator-at+ 1)

(defun generators-in-floor (floor)
  (logand floor #b1111100000))

(defun chips-in-floor (floor)
  (logand floor #b0000011111))

(defun other-generator (chip floor)
  (logxor (logand floor #b1111100000)
          (ash chip 5)))

(defun chips-wo-generator (floor)
  (logand (chips-in-floor floor)
          (lognot (logand (ash floor -5)
                          (chips-in-floor floor)))))

(defun generators-present-p (floor)
  (not (zerop (generators-in-floor floor))))

(defun chip-fried-p (floor)
  ;; Move generators to chips
  (and (chips-wo-generator floor) (generators-present-p floor)))

(chips-wo-generator (logior +sc+ +tg+ +tc+)) ; => 16 (5 bits, #x10, #o20, #b10000)
(chips-wo-generator (logior +sc+ +pg+ +tg+ +tc+)) ; => 16 (5 bits, #x10, #o20, #b10000)

(defun count-bits (integer)
  "The number of 1s in an integer"
  (count-if-not 'null
                (map 'vector (lambda (i) (logbitp i integer))
                     (loop :for i :upto (ceiling (log integer 2))
                           :collect i))))

;; (defun count-bits (integer)
;;   "The number of 1s in an integer"
;;   (count-if-not 'null
;;                 (map 'vector (lambda (i) (logbitp i integer))
;;                      '(0 1 2 3 4 5 6 7 8 9))))

(defparameter +move-bitmasks+ (loop :for num :from 0 :upto #b1111111111
                                    :when (>= 2 (count-bits num) 1)
                                      :collect num))

(defun possible-cargos (floor)
  (remove-duplicates
   (remove-if 'zerop
              (map 'vector (lambda (x)
                             (logand x floor))
                   +move-bitmasks+))))

(defun current-floor (building)
  (ldb (byte 10 (* 10 (1- +elevator-at+)))
       building))

(defun adjacent-floors (floor-number)
  (case floor-number
    (1 '(2))
    (2 '(1 3))
    (3 '(2 4))
    (4 '(3))))

(defun floor-num (num building)
  (ldb (byte 10 (* 10 (1- num)))
       building))

(defun new-floor (elevator-coming-from elevator-at elevator-cargo building)
  ;; Remove cargo from old floor and add it to the new floor.
  (logior (logior (ash elevator-cargo (* 10 (1- elevator-at)))
                  building)
          (logand (ash elevator-cargo (* 10 (1- elevator-coming-from))) building)))

;; Generarar todos y luego filtrar las fritas y las vistas
(defun available-moves (elevator-at building)
  (let ((possible-cargos (possible-cargos (floor-num elevator-at building)))
        (possible-floors (adjacent-floors elevator-at))
        (moves ()))
    (loop :for elevator-destination :in possible-floors
          :do (loop :for cargo :across possible-cargos
                    :for possible-building := (new-floor elevator-at elevator-destination cargo building)
                    :unless (chip-fried-p possible-building)
                      :do (format t "Boo!~%")
                         (push (cons elevator-destination possible-building) moves)))
    moves))

(defun print-floor-contents (floor)
  (with-output-to-string (out)
    (let ((objects ()))
      (when (plusp (logand +sg+ floor))
        (push "Strontium Generator" objects))
      (when (plusp (logand +pg+ floor))
        (push "Plutonium Generator" objects))
      (when (plusp (logand +tg+ floor))
        (push "Thulium Generator " objects))
      (when (plusp (logand +rg+ floor))
        (push "Ruthenium Generator" objects))
      (when (plusp (logand +cg+ floor))
        (push "Curium Generator" objects))

      (when (plusp (logand +sc+ floor))
        (push "Strontium microchp" objects))
      (when (plusp (logand +pc+ floor))
        (push "Plutonium microchp" objects))
      (when (plusp (logand +tc+ floor))
        (push "Thulium microchp" objects))
      (when (plusp (logand +rc+ floor))
        (push "Ruthenium microchp" objects))
      (when (plusp (logand +cc+ floor))
        (push "Curium microchp" objects))

      (format out "~{~A~^, ~}" objects))))
