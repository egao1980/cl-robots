(defsystem "cl-robots"
  :version "0.1.0"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ("esrap" "cl-unicode")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Simple robots.txt reader"
  :in-order-to ((test-op (test-op "cl-robots/tests"))))

(defsystem "cl-robots/tests"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ("cl-robots"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-robots"
  :perform (test-op (op c) (symbol-call :rove :run c)))
