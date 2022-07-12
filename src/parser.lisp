(in-package :claculator)

;; (infix-to-lisp 1 '((+ 2) (+ 3) (- 4)))
;; --> (- (+ (+ 1 2) 3) 4)
(defun infix-to-lisp (expr ops-rhs)
  (loop :for result := expr :then `(,op ,result ,rhs)
        :for (op rhs) :in ops-rhs
        :finally (return result)))

(defgrammar maths
  :terminals ((id   "[A-Za-z][A-Za-z0-9]*")
              ;; ;; real must come first to match the longest first.
              ;; (real    "[-+]?[0-9]+\\.[0-9]+([Ee][-+][0-9]+)?")
              (int "[-+]?[0-9]+"))
  :start expr
  :rules ((--> expr
               (seq term    (rep addop term)
                    :action (infix-to-lisp $1 $2)))
          (--> term
               (seq factor  (rep mulop factor)
                    :action (infix-to-lisp $1 $2)))
          (--> factor
               (alt integer
                    ident
                    (seq "(" expr ")" :action $1)))
          (--> addop
               (alt "+" "-") :action (first $1))
          (--> mulop
               (alt "*" "/" (seq "^" :action `(expr "^" ,(third $1)))) :action (first $1))
          (--> ident id :action (intern (string-upcase (second $1))))
          (--> integer int :action (parse-integer (second $1)))))

;; (parse-maths "3*x^2+2*x+1")
;; --> (+ (+ (expr (* 3 x) 2) (* 2 x)) 1)
