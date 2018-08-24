(defpackage "DAY/7"
  (:use "CL"))
(in-package "DAY/7")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2015/7.input")

(deftype token ()
  `(cons keyword t))

(defun tok (type val)
  (cons type val))

#|
Tokens

:gate-id
:number
:then
:logic-gate
|#

(alexa:define-string-lexer instruction-lexer
  ((:num "\\d+")
   (:gate-id "\\w+")
   (:-> "->"))
  ("{{GATE-ID}}" (return (tok :gate-id $@)))
  ("{{NUM}}" (return (tok :number (parse-integer $@))))
  ("->" (return (tok :then nil)))
  ("NOT" (return (tok :logic-gate :not)))
  ("AND" (return (tok :logic-gate :and)))
  ("OR" (return (tok :logic-gate :or)))
  ("RSHIFT" (return (tok :logic-gate :rshift)))
  ("LSHIFT" (return (tok :logic-gate :lshift)))
  ("\\s+" nil))

(defun lex-line (string)
  (loop :with lexer := (instruction-lexer string)
        :for tok := (funcall lexer)
        :while tok
        :collect tok))
(lex-line "lf AND lq -> ls")

(defun lex (in)
  (loop :for line := (read-line in nil 'eof)
        :until (eq 'eof line)
        :append (lex-line line)
        ;; :append '(:instruction-end)
        ))

(with-open-file (in +input+)
  (lex in))

;; (defparameter +wires+ (make-hash-table :test 'equalp))
(defparameter +wires+ ())


;; Assumiendo que no hay referencias a gate ids en el lhs antes que rhs

#|
Parser grammar

<input> := <gate-id> | <num>

<left-side> := NOT <gate-id> -> <gate-id> |
               <gate-id> RSHIFT <num> |
               <gate-id> LSHIFT <num> |
               <input> AND <input> |
               <input> OR <input> |
<left-side> -> <gate-id>
|#

(yacc:define-parser instruction-parser
  (:start-symbol expression)
  (:terminals :gate-id :number :then :logic-gate :not :rshift :lshift :and :or :then)
  (expression
   (left-side :then gate-id (lambda (x y z) (list x :foobar z))))

  ;; (-> (:then nil))

  (input gate-id
         number)

  (gate-id :gate-id string)
  (number (:number string-parsr))
  
  (left-side
   not-gate-expression
   and-gate-expression
   or-gate-expression
   rshift-gate-expression
   lshift-gate-expression)

  (not-gate (:logic-gate :not))
  (and-gate (:logic-gate :and))
  (or-gate (:logic-gate :or))
  (rshift-gate (:logic-gate :rshift))
  (lshift-gate (:logic-gate :lshift))

  (not-gate-expression (not-gate input))
  (and-gate-expression (input and-gate input))
  (or-gate-expression (input or-gate input))
  (rshift-gate-expression (input :rshift number))
  (lshift-gate-expression (input :rshift number))
  
  
  
  (term))

