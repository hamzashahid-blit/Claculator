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
  ("%" percentage)
  ("mod" mod)
  ("rem" rem))

(testsuite lexer
  (testcase (lexer find-operator)
            :actual (c::find-operator "%" *clac-ops-test*)
            :expected '("%" percentage)
            :equal 'equalp
            :info "Find operator giving list in vector containing operator mapping")
  (testcase (lexer operators-count)
            :actual (c::operators-count *clac-ops-test*)
            :expected 7
            :equal '=
            :info "Count the number of operators"))
