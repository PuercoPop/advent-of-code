#lang racket

(define +input+ "/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/9.input")

(define (find-char c s)
  (for/first ([x s]              ; for each character in the string c
              [i (in-naturals)]  ; counts 0, 1, 2, ...
              #:when (char=? c x))
    i))

(define +som+ #\()
(define +eom+ #\))

(define (marker-start-p string)
  (char=? +som+ (string-ref string 0)))

(define (read-marker string)
  (map string->number (string-split (substring string 1 (find-char +eom+ string))
                                     "x")))

(define (%count-characters string count)
  (cond ((string=? "" string) count)
        ((marker-start-p string)
         ;; =>
         (match (read-marker string)
                 [(list eat-num times) (%count-characters (substring string eat-num)
                                                          (+ count
                                                             (* times
                                                                (%count-characters
                                                                 (substring string 0 eat-num)
                                                                 0))))]))
        (#t (%count-characters (substring string 1) (+ 1 count)))))

(define (count-characters string)
  (%count-characters string 0))