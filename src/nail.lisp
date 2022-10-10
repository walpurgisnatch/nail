(in-package :cl-user)
(defpackage nail
  (:use :cl
        :nail.utils))

(in-package :nail)

(defun dot (&rest vectors)
  (reduce #'+ (apply #'mapcar #'* vectors)))

(defun sum-of-squares(v)
  (dot v v))

(defun magnitude (v)
  (sqrt (sum-of-squares v)))

(defun distance (v, w)
  (magnitude (mapcar #'- v w)))

(defun mean (list)
  (/ (reduce #'+ list) (length list)))

(defun de-mean (list)
  (mapcar-2 - (mean list) list))

(defun variance (list)
  (let ((len (length list))
        (deviations (de-mean list)))
    (/ (sum-of-squares deviations) (1- len))))

(defun standard-deviation (list)
  (sqrt (variance list)))

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

(defun mode (list)
   ;TODO
  )

(defun count (list)
  ;TODO
  )
