
(defsystem "nail"
  :version "0.1.8"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on ("cl-ppcre"
               "eazy-gnuplot"
               "cl-reexport")
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "plotting")
                 (:file "linalg" :depends-on ("utils"))
                 (:file "statistics" :depends-on ("linalg"))
                 (:file "nail" :depends-on ("statistics")))))
  :description "Data science operations library"
  :in-order-to ((test-op (test-op "nail/tests"))))
