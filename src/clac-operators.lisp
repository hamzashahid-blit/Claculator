(in-package :claculator)

(defun gen-define-operators (name &rest tokens)
  "Defines Operators with the symbol NAME and tokens, TOKENS."
  `(defparameter ,name
     (quote ,@tokens))) 

(defmacro define-operators (name (key value) &rest other-pairs)
  (gen-define-operators name (cons (list key value) other-pairs)))

;; ---------------    
;; Abstractions

(defun operators-count (operators)
  (length operators))

(defun find-operator (str operators &key (test #'string=))
  "Tries to find STR in OPERATORS based on TEST"
  (assoc str operators :test test))

(defun add-operator (str token operators)
  (append operators (list (list str token))))

;; ---------------
    
(defmethod percentage (arg)
  (/ arg 100))

;;; TODO: FIX; Gives error as gen-define-operators isn't compiled before macro expansion
;; (define-operators *clac-ops*
;;   ("+" +)
;;   ("-" -)
;;   ("*" *)
;;   ("/" /)
;;   ("%" percentage)
;;   ("mod" mod)
;;   ("rem" rem))
