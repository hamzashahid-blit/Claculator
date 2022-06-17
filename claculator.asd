(defsystem "claculator"
  :version "0.1.0"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("mcclim"
               "bt-semaphore"  ; Todo or not to do?
               "str")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "Calculator made purely in Common Lisp for CLOSOS"
  :in-order-to ((test-op (test-op "claculator/tests"))))

(defsystem "claculator/tests"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("claculator"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for claculator"
  :perform (test-op (op c) (symbol-call :rove :run c)))
