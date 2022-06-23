(asdf:defsystem "claculator"
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
                 (:file "clac-operators")
                 (:file "lexer")
                 (:file "main"))))
  :description "Calculator made purely in Common Lisp for CLOSOS"
  :in-order-to ((asdf:test-op (asdf:test-op "claculator/tests"))))

(asdf:defsystem "claculator/tests"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("claculator"
               "cl-naive-tests")
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "Test system for claculator"
  :perform (asdf:test-op (op c)
             (declare (ignore c))
             (symbol-call :cl-naive-tests :run)
             (symbol-call :cl-naive-tests :report)))
