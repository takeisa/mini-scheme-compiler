(defsystem "mini-scheme-compiler"
  :version "0.1.0"
  :author "Satoshi Takei"
  :license ""
  :depends-on ("alexandria")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Mini Scheme compiler"
  :in-order-to ((test-op (test-op "mini-scheme-compiler/tests"))))

(defsystem "mini-scheme-compiler/tests"
  :author "Satoshi Takei"
  :license ""
  :depends-on ("mini-scheme-compiler"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for mini-scheme-compiler"
  :perform (test-op (op c) (symbol-call :rove :run c)))
