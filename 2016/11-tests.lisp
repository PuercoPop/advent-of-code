(defpackage "DAY/11-TESTS"
  (:use "DAY/11"
        "FIASCO"))
(in-package "DAY/11-TESTS")

(availabe-moves +building+)

(chip-fried-p #\t (list #\s #\S))
(chip-fried-p #\t (list #\s #\S #\T))

(has-other-generator #\t (list #\s #\S #\T))
(has-other-generator #\t (list #\s #\T))
