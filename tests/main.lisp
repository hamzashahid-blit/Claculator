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
  ("+" +)
  ("-" -)
  ("*" *)
  ("/" /)
  (">=" >=)
  ("%" percentage)
  ("mod" mod))

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

(testsuite lexer
  (testcase (lexer tokenize-number)
            :actual (with-input-from-string (stream "198273)llo*@&#")
                      (c::tokenize-number stream))
            :expected 198273
            :equal '=
            :info "Take out a number from a stream as a string giving a tokenized number.")
  (testcase (lexer tokenize-number-char)
            :actual (with-input-from-string (stream "d198273)llo*@&#")
                      (c::tokenize-number stream))
            :expected nil
            :equal 'eql
            :info "Tokenize a char giving NIL as it shouldn't parse anything other than digits.")
  (testcase (lexer tokenize-number-whole)
            :actual (with-input-from-string (stream "12345")
                      (c::tokenize-number stream))
            :expected 12345
            :equal '=
            :info "Tokenize one whole number.")
  (testcase (lexer tokenize-operator)
            :actual (with-input-from-string (stream ">=4")
                      (c::tokenize-operator stream *clac-ops-test*))
            :expected '>=
            :equal 'eql
            :info "Tokenize a 2 character operator.")
  (testcase (lexer tokenize-two-operators)
            :actual (with-input-from-string (stream "+-4")
                      (c::tokenize-operator stream *clac-ops-test*))
            :expected '+
            :equal 'eql
            :info "Tokenize the first of two operators put together.")
  (testcase (lexer tokenize-operator-digit)
            :actual (with-input-from-string (stream "5+-4")
                      (c::tokenize-operator stream *clac-ops-test*))
            :expected nil
            :equal 'eql
            :info "Tokenize a digit giving NIL as it shouldn't parse digits.")
  (testcase (lexer tokenize-operator-whole)
            :actual (with-input-from-string (stream ">=")
                      (c::tokenize-operator stream *clac-ops-test*))
            :expected '>=
            :equal 'eql
            :info "Tokenize one whole operator.")
  (testcase (lexer tokenize-expression)
            :actual (with-input-from-string (stream "2+13+-422*56% /5")
                      (c::tokenize-expr stream *clac-ops-test*))
            :expected '(2 + 13 + - 422 * 56 PERCENTAGE / 5)
            :equal 'equal
            :info "Tokenize an ULTIMATE expression.")
  (testcase (lexer tokenize-expression-number)
            :actual (with-input-from-string (stream "2291")
                      (c::tokenize-expr stream *clac-ops-test*))
            :expected (list 2291)
            :equal 'equal
            :info "Tokenize a number for an expression.")
  (testcase (lexer tokenize-expression-operator-multichar)
            :actual (with-input-from-string (stream ">=")
                      (c::tokenize-expr stream *clac-ops-test*))
            :expected '(>=)
            :equal 'equal
            :info "Tokenize a multi-character operator for an expression.")
  (testcase (lexer tokenize-expression-operator-single-char)
            :actual (with-input-from-string (stream "*")
                      (c::tokenize-expr stream *clac-ops-test*))
            :expected '(*)
            :equal 'equal
            :info "Tokenize a single-character operator for an expression.")
  (testcase (lexer tokenize-expression-empty)
            :actual (with-input-from-string (stream "")
                      (c::tokenize-expr stream *clac-ops-test*))
            :expected nil
            :equal 'equal
            :info "Tokenize an empty expression."))
