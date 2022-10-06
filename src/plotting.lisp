(in-package :cl-user)
(defpackage nail.plotting
  (:use :cl :eazy-gnuplot))

(in-package nail.plotting)


;; (with-plots (s :debug t)
;;   (gp-setup :xlabel "x-label"      
;;             :ylabel "y-label"
;;             :output "sample.png" 
;;             :terminal :png
            
;;             :yrange :|[-1.1:1.1]|
;;             :xrange :|[(- pi):pi]|
;;             )
;;   (gp :set :grid)

;;   (plot "sin(x) title \"sinux\"  lc rgb \"red\", cos(x)  title \"cosinus\" lc rgb \"green\""))
