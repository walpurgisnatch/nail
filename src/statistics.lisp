(in-package :cl-user)
(defpackage nail.statistics
  (:use :cl
        :nail.utils
        :nail.linalg)
  (:export :mean
           :median
           :quantile
           :variance
           :covariance
           :standard-deviation
           :correlation))

(in-package :nail.statistics)

(defun mean (list)
  (/ (reduce #'+ list) (length list)))

(defun median (list)
  (let* ((sorted (sort (copy-seq list) #'<))
         (len (length list))
         (middle (floor len 2)))
    (if (oddp len)
        (elt sorted middle)
        (/ (+ (elt sorted middle)
              (elt sorted (1- middle)))
           2))))

(defun quantile (list p)
  (let ((sorted (sort (copy-seq list) #'<)))
    (elt sorted (round (* (1- (length list)) p)))))

(defun de-mean (list)
  (mapcar-2 - (mean list) list))

(defun variance (list)
  (let ((len (length list))
        (deviations (de-mean list)))
    (/ (sum-of-squares deviations) (1- len))))

(defun covariance (x y)
  (/ (dot (de-mean x) (de-mean y)) (1- (length x))))

(defun standard-deviation (list)
  (sqrt (variance list)))

(defun correlation (x y)
  (let ((stdev-x (standard-deviation x))
        (stdev-y (standard-deviation y)))
    (if (and (> stdev-x 0) (> stdev-y 0))
        (/ (/ (covariance x y) stdev-x) stdev-y))))


;; (defun mode (list)
;;    ;TODO
;;   )
