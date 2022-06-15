(defpackage claculator/tests/main
  (:use :cl
        :claculator
        :rove))
(in-package :claculator/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :claculator)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
