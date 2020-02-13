(defpackage cl-robots/tests/main
  (:use :cl
        :cl-robots
        :rove))
(in-package :cl-robots/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-robots)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
