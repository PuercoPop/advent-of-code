(defpackage "DAY/11"
  (:use "CL"
        "ALEXANDRIA"))
(in-package "DAY/11")

;; 4 kinds of objects

;; WORLD = Building + Elevator (42 bits)
;; Building = Floor4 + Floor3 + Floor2 + Floor1 (40 bits)
;; Floor = 10 bits
;; Elevator 2 bits

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

(defparameter +initial-position+ (ash (logior (ash +tc+ 20)
                                                (ash (logior +tg+ +rg+ +rc+ +cg+ +cc+) 10)
                                                (logior +sg+ +sc+ +pg+ +pc+))
                                        2))

(defparameter +winning-position+ (logior 3 (ash (ash (logior +sg+ +pg+ +tg+ +rg+ +cg+ +sc+ +pc+ +tc+ +rc+ +cc+)
                                                     30)
                                                2)))

(defun buildingp (building)
  (> (expt 2 40) building))

(defun floorp (floor)
  "Check if the integer could be a floor"
  (> (expt 2 10) floor))

(defun elevatorp (elevator)
  (> (expt 2 2) elevator))


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
  (if (zerop integer)
      0
      (count-if-not 'null
                    (map 'vector (lambda (i) (logbitp i integer))
                         (loop :for i :upto (ceiling (log integer 2))
                               :collect i)))))

(defun current-floor (world)
  "Returns the current floor, ie where the elevator currently is."
  (ldb (byte 2 0) world))

(defun adjacent-floors (world)
  (case (current-floor world)
    (0 '(1))
    (1 '(0 2))
    (2 '(1 3))
    (3 '(2))))

(defun floor-number (num world)
  "Returns the floor at NUM."
  (ldb (byte 10 (+ 2 (* 10 num)))
       world))

(defparameter +move-bitmasks+ (loop :for num :from 0 :upto #b1111111111
                                    :when (>= 2 (count-bits num) 1)
                                      :collect num))

(defun possible-cargos (world)
  "Return the possible objects for the elevator to transport."
  (let ((current-floor (floor-number (current-floor world) world)))
    (remove-duplicates
     (remove-if 'zerop
                (map 'vector (lambda (x)
                               (logand x current-floor))
                     +move-bitmasks+)))))

(defun print-as-bits (int &optional (stream *standard-output*))
  (let ((*print-base* 2)
        (*print-radix* t))
    (format stream "~&~S" int)))
(defun print-as-bits (int &optional (stream *standard-output*))
  (flet ((transform (bool)
           (if bool
               1
               0)))
    (let (result)
      (dotimes (i (1+ (ceiling (log (abs int) 2))))
        (push (logbitp i int) result))
      (format stream "#b~{~A~}" (mapcar #'transform result)))))

;; De aca para arriba functiona
(defun place-floor-at (floor-number floor)
  (ash floor (+ 2 (* 10 floor-number))))

;; No funciona, test with (print-world (clear-floor +initial-position+)). Deberia mostrar el primer piso vacio
(defun clear-floor (world)
  (let* ((current-floor-number (current-floor world))
         (current-floor (floor-number current-floor-number world))
         (clear-bitmask (lognot current-floor)))
    (logxor world
            (place-floor-at current-floor-number clear-bitmask))))

;; WIP
(defun remove-cargo (cargo world)
  ;; Cargo is a floor
  (let ((floor (logxor cargo (floor-number (current-floor world) world))))
    (logand (clear-floor world)
            (place-floor-at (current-floor world) floor))))

(defun move (cargo floor-number world)
  (remove-cargo cargo world)
  (insert-cargo cargo floor-number world))

;; Later Prune
(defun next-states (world)
  (let ((possible-cargos (possible-cargos world))
        (possible-destinations (adjacent-floors world)))
    (loop :for elevator-destination :in possible-destinations
          :do (loop :for cargo :across possible-cargos
                    :for next-world := (move cargo elevator-destination world)
                    :collect next-world))))


;; Instrospection

(defun print-floor (floor &optional (stream *standard-output*))
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

    (format stream "~{~A~^, ~}" objects)))

(defun print-world (world &optional (stream *standard-output*))
  (format stream "Elevator at ~A~%" (current-floor world))
  (dotimes (i 4)
    (format stream "Floor #~A: " (- 3 i))
    (print-floor (floor-number (- 3 i) world) stream)
    (format stream "~%")))
