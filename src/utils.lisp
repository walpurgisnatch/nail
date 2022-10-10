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
           :elt-randoms
           :parse-float
           :round-to
           :mapcar-2
           :list-or-car))

(in-package :nail.utils)

(defmacro list-or-car (&body body)
  `(let ((data ,@body))
     (if (cdr data)
         data
         (car data))))

(defmacro mapcar-2 (fun arg list)
  `(mapcar #'(lambda (el) (,fun el ,arg)) ,list))

(defun parse-float (string)
  (list-or-car 
    (let ((*read-eval* nil))
      (with-input-from-string (stream string)
        (loop for number = (read stream nil nil)
              while (and number (numberp number)) collect number)))))

(defun round-to (float decimals)
  (parse-float (format nil "~,vf" decimals float)))

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
