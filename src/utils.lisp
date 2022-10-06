(in-package :cl-user)
(defpackage nail.utils
  (:use :cl)
  (:export :mappend
           :sort-closest-to
           :find-max
           :find-min
           :find-all
           :range
           :transpose
           :flatten
           :range-random
           :random-range
           :elt-random
           :elt-randoms))

(in-package :nail.utils)

(defun mappend (fn list)
    (apply #'append (mapcar fn list)))

(defun sort-closest-to (list element)
  (sort list #'< :key (lambda (x) (abs (- x element)))))

(defun find-max (list)
  (apply #'max list))

(defun find-min (list)
  (apply #'min list))

(defun find-all (fun list)
  (remove-if-not fun list))

(defun range (max &key (min 0) (step 1))
  (loop for i from min below max by step
        collect i))

(defun transpose (&rest lists)
  (apply #'mapcar #'list lists))

(defun flatten (list &optional acc)
  (cond ((null list) acc)
        ((atom list) (cons list acc))
        (t (flatten (car list) (flatten (cdr list) acc)))))

;; random works

(defun range-random (n min max)
  (loop for i from 1 to n
        collect (random-range min max)))

(defun random-range (min max)
  (+ min (random (- max min))))

(defun elt-random (list)
  (elt list (random (length list))))

(defun elt-randoms (list n)
  (loop for i from 1 to n
        collect (elt-random list)))
