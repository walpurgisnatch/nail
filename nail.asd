(defsystem "nail"
  :version "0.1.0"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on (cl-ppcre)
  :components ((:module "src"
                :components
                :serial t
                ((:file "utils")
                 (:file "nail"))))
  :description ""
  :in-order-to ((test-op (test-op "nail/tests"))))
