(defpackage mini-scheme-compiler/tests/main
  (:use :cl
        :mini-scheme-compiler
        :rove))
(in-package :mini-scheme-compiler/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :mini-scheme-compiler)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
