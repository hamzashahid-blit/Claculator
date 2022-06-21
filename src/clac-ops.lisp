(in-package :claculator)

(defun gen-define-operators (name &rest tokens)
  "Defines Operators with the symbol NAME and tokens, TOKENS."
  `(defparameter ,name
     (coerce (quote ,@tokens) 'vector))) 

(defmacro define-operators (name (key value) &rest other-pairs)
  (gen-define-operators name (cons (list key value) other-pairs)))

;; ---------------    
;; Abstractions

(defun operators-count (operators)
  (length operators))

(defun find-operator (str operators &key (test #'string=))
  "Tries to find STR in OPERATORS based on TEST"
  (find str operators :test test))

;; ---------------
    
(defmethod percentage (arg)
  (/ arg 100))

(define-operators *clac-ops*
  ("+" +)
  ("-" -)
  ("*" *)
  ("/" /)
  ("%" percentage)
  ("mod" mod)
  ("rem" rem))
