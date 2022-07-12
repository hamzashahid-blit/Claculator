(in-package :claculator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-define-operators (name &rest tokens)
    "Defines Operators with the symbol NAME and tokens, TOKENS."
    `(defparameter ,name (quote ,@tokens)))) 

(defmacro define-operators (name ((symbol token arity notation)) &rest other-pairs)
  (gen-define-operators name (cons (list symbol token arity notation) other-pairs)))

;; --------------- 
;; Extractions

;; (defun extract-operator-part (operators part)
;;   (let ((operator-parts (gensym)))
;;     `(let ((,operator-parts '()))
;;        ,(loop :for precedent :in operators
;;               :do (loop :for operator :in precedent
;;                         :do (push part operator-parts)))
;;        (nreverse ,operator-parts))))

(defun extract-operator-pairs (operators)
  (let ((operator-pairs '()))
    (loop :for precedent :in operators
          :do (loop :for operator :in precedent
                    :do (push (list (first operator) (second operator))
                              operator-pairs)))
    (nreverse operator-pairs)))

(defun extract-operator-symbols (operators)
  (let ((operator-symbols '()))
    (loop :for precedent :in operators
          :do (loop :for operator :in precedent
                    :do (push (first operator) operator-symbols)))
    (nreverse operator-symbols)))

(defun extract-operator-tokens (operators)
  (let ((operator-tokens '()))
    (loop :for precedent :in operators
          :do (loop :for operator :in precedent
                    :do (push (second operator) operator-tokens)))
    (nreverse operator-tokens)))

(defun extract-operator-arities (operators)
  (let ((operator-arities '()))
    (loop :for precedent :in operators
          :do (loop :for operator :in precedent
                    :do (push (third operator) operator-arities)))
    (nreverse operator-arities)))

(defun extract-operator-notations (operators)
  (let ((operator-notations '()))
    (loop :for precedent :in operators
          :do (loop :for operator :in precedent
                    :do (push (fourth operator) operator-notations)))
    (nreverse operator-notations)))

;; ---------------    
;; Abstractions

(defun operators-count (operators)
  (length operators))

(defun find-operator (str operators &key (test #'string=))
  "Tries to find STR in OPERATORS based on TEST"
  (assoc str (extract-operator-pairs operators) :test test))

(defun add-operator (str token operators)
  (append operators (list (list str token))))

;; ---------------
    
(defmethod percentage (arg)
  (/ arg 100))

;; ;; Operator order declare operator precedence and sublists tell the same precedence.
;; ;; ARGS:
;; ;; 1st: The operator the LEXER would see
;; ;; 2nd: The Token/Function associated with that operator
;; ;; 3rd: Arity of each operator i.e how many arguments it would take
;; ;; 4th: If the operator goes in the middle, before, or after (:infix, :prefix, :postfix)
;; (define-operators *clac-ops*
;;   ((("%" percentage 1 :postfix))
;;    (("+" + 2 :infix)
;;     ("-" - 2 :infix))
;;    (("*" * 2 :infix)
;;     ("/" / 2 :infix)
;;     ("mod" mod 2 :infix))))
