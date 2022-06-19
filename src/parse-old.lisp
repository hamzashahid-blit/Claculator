(in-package :claculator)

;; TODO: parse 2+3*4+5 into (+ (+ 2 (* 3 4)) 5)
;; TODO: parse 3x + 5/7^3 into (+ (* 3 x) (/ 5 (^ 7 3)))


;; ;; #\+, "+", fboundp
;; (defun is-char-operator? (op)
;;   (check-type op (or string character))
;;   ;; (handler-case (fboundp (intern (string op)))
;;   ;;   (error () (return-from is-char-operator? nil)))
;;   ;; t
;;   ;(eql :inherited (nth-value 1 (intern (string op)))))
;;   )

;; 2+3*4+5
;; 2 + (* 3 4) + 5
;; (+ 2 (* 3 4)) + 5
;; (+ (+ 2 (* 3 5)) 5)

;; "2+3*4+5" -> '(2 + 3 * 4 + 5)

(defun parse-expr (str-expr)
  "Takes in an algebraic string and parses it to a list of symbols and numbers."
  (let ((list nil))
    (loop :for op :across str-expr
      :do (if (digit-char-p op)
            (push (parse-integer (string op)) list)
            (push (intern (string op)) list))
      :finally (return-from parse-expr (reverse list)))))

;; ;; '(2 + 3 * 4 + 5)
;; (defun evaluate-expr (expr)
;;   "Takes in an expression as a list of symbols and numbers and evaluates it."
;;   (loop :for sym :in expr
;;         :for i :from 0
;;     :do (if ())))

;; (defun evaluator (expr)
;;   "Takes in an algebraic string expression and evaluates it."
;;   (loop :named mult-loop :for op :across expr
;;     :for i :from 0
;;     :when (char= #\* op)
;;     :do (let* ((arg1 (elt *expr* (1- i)))
;;                 (arg2 (elt *expr* (1+ i)))
;;                 (char-expr (list op arg1 arg2))
;;                 (answer (princ-to-string (eval-char-expr char-expr)))
;;                 (to-replace (str:concat (string arg1) (string op) (string arg2))))
;;           (setf expr (str:replace-first to-replace answer expr))))
;;   expr)

(defun char->str (chars)
  (mapcar #'string chars))

(defun char-expr->expr (char-expr)
  "Converts a list of chars into something that could be evaluated."
  (let* ((str-expr (char->str char-expr))
         (args (mapcar #'parse-integer (cdr str-expr)))
		 (op (intern (car str-expr))))
    (push op args)))

(defun eval-char-expr (char-expr)
  "Takes in a list of chars and evaluates it."
  (eval (char-expr->expr char-expr)))
