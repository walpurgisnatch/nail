
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
  :description "library providing convenient functions for working with linalg, statistics and probability.")
