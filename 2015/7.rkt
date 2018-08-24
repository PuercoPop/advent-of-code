#lang racket

(require parser-tools/lex)
(require parser-tools/lex-sre)
(require parser-tools/yacc)

#|
lexer
(alexa:define-string-lexer instruction-lexer
  ((:num "\\d+")
   (:gate-id "\\w+")
   (:-> "->"))
  ("{{GATE-ID" (return (tok :gate-id $@)))
  ("{{NUM}}" (return (tok :number (parse-integer $@))))
  ("->" (return (tok :then nil)))
  ("NOT" (return (tok :logic-gate :not)))
  ("AND" (return (tok :logic-gate :and)))
  ("OR" (return (tok :logic-gate :or)))
  ("RSHIFT" (return (tok :logic-gate :rshift)))
  ("LSHIFT" (return (tok :logic-gate :lshift)))
  ("\\s+" nil))
|#

(define-empty-tokens empty-instruction-tokens
  (then))

(define-tokens instruction-tokens
  (gate-id number logic-gate))

(define instruction-lexer 
  (lexer
 ((eof) 'end-marker)
 (#\Newline (instruction-lexer input-port))
 (whitespace (instruction-lexer input-port))

 ((+ alphabetic) (cons (token-gate-id lexeme)
                       (instruction-lexer input-port)))
 ((+ numeric) (cons (token-number (string->number lexeme))
                    (instruction-lexer input-port)))
 
 ("NOT" (cons (token-logic-gate 'not)
              (instruction-lexer input-port)))
 ("AND" (cons (token-logic-gate 'and)
              (instruction-lexer input-port)))
 ("OR" (cons (token-logic-gate 'or)
             (instruction-lexer input-port)))
 ("LSHIFT" (cons (token-logic-gate 'lshift)
                 (instruction-lexer input-port)))
 ("RSHIFT" (cons (token-logic-gate 'rshift)
                 (instruction-lexer input-port)))
 ("RSHIFT" (cons (token-logic-gate 'rshift)
                 (instruction-lexer input-port)))

 ("->" (cons (token-then)
             (instruction-lexer input-port)))))

(define (lex path)
  (let ((in (open-input-file path)))
    (instruction-lexer in)))

;; (lex "/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2015/7.input")

#|

<input> := <gate-id> | <num>

<left-side> := NOT <gate-id> -> <gate-id> |
               <gate-id> RSHIFT <num> |
               <gate-id> LSHIFT <num> |
               <input> AND <input> |
               <input> OR <input> |
<left-side> -> <gate-id>

|#

(define instruction-parser
  (parser
   (start exp)
   (end EOF)
   (tokens )
   (grammar
    (exp
