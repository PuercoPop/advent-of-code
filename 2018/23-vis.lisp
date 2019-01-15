(defpackage "AOC/23-VIS"
  (:use "CLIM"
        "CLIM-LISP")
  (:export
   #:main))
(in-package "AOC/23-VIS")

(define-application-frame aoc-23 ()
  ()
  (:panes (map :application
               :display-function 'draw-map
               :scroll-bars nil)
          (int :interactor))
  (:menu-bar menu-command-table)
  (:layouts
   (:default (vertically ()
               (scrolling (:height 600 :Width 600 :scroll-bars t)
                 map)
               int))))

(defun draw-bot (bot stream &key (size 7))
  (let* ((pos (aoc/23::pos bot))
         (x (* 10 (aoc/23::x pos)))
         (y (* 10 (aoc/23::y pos)))
         (dx (+ size x))
         (dy (+ size y)))
    (draw-rectangle* stream
                     x dx
                     y dy
                     :ink +yellow+)))

;; How to make a 3d-vis
(defmethod draw-map ((map aoc-23) stream &key &allow-other-keys)
  (loop :for bot :in aoc/23::+ex2-bots+
        :do (draw-bot bot stream)))


(define-aoc-23-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(make-command-table 'menu-command-table
                    :errorp nil
                    :menu '(("Quit" :command com-quit)))

(defun main ()
  (run-frame-top-level (make-application-frame 'aoc-23)))
