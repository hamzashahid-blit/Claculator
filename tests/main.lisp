(in-package :claculator/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :claculator)' in your Lisp.
;;       And then run (run-tests)

(defvar *disable-failure-tests* nil)
(defvar *use-color* t)

;;; NOTE: If you use SLY, and the output of format-results is not showing in full, you can run:
;;;       (setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)
(defun run-tests ()
  (report (run))
  (format-results :text *suites-results*))

(c::define-operators *clac-ops-test*
  '((("%" percentage 1 :postfix))
    (("+" + 2 :infix)
     ("-" - 2 :infix))
    (("*" * 2 :infix)
     ("/" / 2 :infix)
     ("mod" mod 2 :infix))
    ((">=" >= 2 :infix))))

(testsuite clac-operators
  (testcase (clac-operators find-operator)
            :actual (c::find-operator "%" *clac-ops-test*)
            :expected '("%" percentage)
            :equal 'equalp
            :info "Find operator giving list in vector containing operator mapping.")
  (testcase (clac-operators operators-count)
            :actual (c::operators-count *clac-ops-test*)
            :expected 7
            :equal '=
            :info "Count the number of operators.")
  (testcase (clac-operators add-operator)
            :actual (c::add-operator "==" '== *clac-ops-test*)
            :expected '(("+" +) ("-" -) ("*" *) ("/" /) (">=" >=) ("%" PERCENTAGE) ("mod" MOD) ("==" ==))
            :equal 'equal
            :info "Add an operator to already existing operators defined by define-operators."))

(testsuite clac-operator-extraction
  (testcase (clac-operator-extraction extract-operator-pairs)
            :actual (c::extract-operator-pairs *clac-ops-test*)
            :expected '(("+" +) ("-" -) ("*" *) ("/" /) ("%" percentage) ("mod" mod))
            :equal 'equal
            :info "Extract operator pairs, giving an association list with a mapping from string-symbols to tokens.")
  (testcase (clac-operator-extraction extract-operator-symbols)
            :actual (c::extract-operator-symbols *clac-ops-test*)
            :expected '("+" "-" "*" "/" "%" "mod")
            :equal 'equal
            :info "Extract operator symbols, giving the string form of the operator.")
  (testcase (clac-operator-extraction extract-operator-tokens)
            :actual (c::extract-operator-tokens *clac-ops-test*)
            :expected '(+ - * / percentage mod)
            :equal 'equal
            :info "Extract operator tokens, giving the functions corresponding to the operator symbols.")
  (testcase (clac-operator-extraction extract-operator-arities)
            :actual (c::extract-operator-arities *clac-ops-test*)
            :expected '(2 2 2 2 1 2)
            :equal 'equal
            :info "Extract operator arities, giving the number of arguments each operator takes.")
  (testcase (clac-operator-extraction extract-operator-notations)
            :actual (c::extract-operator-notations *clac-ops-test*)
            :expected '(:INFIX :INFIX :INFIX :INFIX :POSTFIX :INFIX)
            :equal 'equal
            :info "Extract operator notations, giving where the operator is placed relative to its arguemnts."))

(testsuite tokenize-numbers
  (testcase (tokenize-numbers tokenize-number)
            :actual (with-input-from-string (stream "198273)llo*@&#")
                      (c::tokenize-number stream))
            :expected 198273
            :equal '=
            :info "Take out a number from a stream as a string giving a tokenized number.")
  (testcase (tokenize-numbers tokenize-number-char)
            :actual (with-input-from-string (stream "d198273)llo*@&#")
                      (c::tokenize-number stream))
            :expected nil
            :equal 'eql
            :info "Tokenize a char giving NIL as it shouldn't parse anything other than digits.")
  (testcase (tokenize-numbers tokenize-number-whole)
            :actual (with-input-from-string (stream "12345")
                      (c::tokenize-number stream))
            :expected 12345
            :equal '=
            :info "Tokenize one whole number."))

(testsuite lexer-operators
  (testcase (lexer-operators tokenize-operator)
            :actual (with-input-from-string (stream ">=4")
                      (c::tokenize-operator stream *clac-ops-test*))
            :expected '>=
            :equal 'eql
            :info "Tokenize a 2 character operator.")
  (testcase (lexer-operators tokenize-two-operators)
            :actual (with-input-from-string (stream "+-4")
                      (c::tokenize-operator stream *clac-ops-test*))
            :expected '+
            :equal 'eql
            :info "Tokenize the first of two operators put together.")
  (testcase (lexer-operators tokenize-operator-digit)
            :actual (with-input-from-string (stream "5+-4")
                      (c::tokenize-operator stream *clac-ops-test*))
            :expected nil
            :equal 'eql
            :info "Tokenize a digit giving NIL as it shouldn't parse digits.")
  (testcase (lexer-operators tokenize-operator-whole)
            :actual (with-input-from-string (stream ">=")
                      (c::tokenize-operator stream *clac-ops-test*))
            :expected '>=
            :equal 'eql
            :info "Tokenize one whole operator."))

(testsuite lexer-expression
  (testcase (lexer-expression tokenize-expression)
            :actual (with-input-from-string (stream "2+13+-422*56% /5")
                      (c::tokenize-expression stream *clac-ops-test*))
            :expected '(2 + 13 + - 422 * 56 PERCENTAGE / 5)
            :equal 'equal
            :info "Tokenize an ULTIMATE expression.")
  (testcase (lexer-expression tokenize-expression-number)
            :actual (with-input-from-string (stream "2291")
                      (c::tokenize-expression stream *clac-ops-test*))
            :expected (list 2291)
            :equal 'equal
            :info "Tokenize a number for an expression.")
  (testcase (lexer-expression tokenize-expression-operator-multichar)
            :actual (with-input-from-string (stream ">=")
                      (c::tokenize-expression stream *clac-ops-test*))
            :expected '(>=)
            :equal 'equal
            :info "Tokenize a multi-character operator for an expression.")
  (testcase (lexer-expression tokenize-expression-operator-single-char)
            :actual (with-input-from-string (stream "*")
                      (c::tokenize-expression stream *clac-ops-test*))
            :expected '(*)
            :equal 'equal
            :info "Tokenize a single-character operator for an expression.")
  (testcase (lexer-expression tokenize-expression-empty)
            :actual (with-input-from-string (stream "")
                      (c::tokenize-expression stream *clac-ops-test*))
            :expected nil
            :equal 'equal
            :info "Tokenize an empty expression."))

;; (testsuite parser
;;   (testcase (parser parse-expression-ultimate
;;               :actual (with-input-from-string (stream "2+13+-422*56% /5")
;;                         (c::parse-expression-ultimate
;;                           (c::tokenize-expression stream *clac-ops-test*)))
;;               :expected '(+ 2 (+ 13 (* -422 (/ (PERCENTAGE 56) 5))))
;;               :equal 'equal
;;               :info "Parse an ULTIMATE tokenized expression"))
