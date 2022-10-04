(in-package :cl-user)
(defpackage nail.linalg
  (:use :cl)
  (:export :matrix
   :matrix-at
           :|setf matrix-at|
           :copy-matrix
   :transposed
           :matrix-print
           :m*))

(in-package :nail.linalg)

(defclass matrix ()
  ((rows
    :initarg :rows
    :initform (error "no :rows specified")
    :reader matrix-rows)
   (cols
    :initarg :cols
    :initform (error "no :cols specified")
    :reader matrix-cols)
   (data
    :initarg :data
    :accessor matrix-data)))

(defmethod initialize-instance :after ((m matrix) &key generator)
  (assert (< 0 (matrix-rows m))
          nil
          ":rows must be > 0.")
  (assert (< 0 (matrix-cols m))
          nil
          ":cols must be > 0.")
  (if (slot-boundp m 'data)
      (progn
        (assert (= (length (matrix-data m)) (* (matrix-rows m) (matrix-cols m)))
                nil
                ":data dimension should be ~d."
                (* (matrix-rows m) (matrix-cols m)))
        (assert (not generator)
                nil
                ":data and :generator may not be specified at the same time."))
      (if (functionp generator)
          (progn
            (setf (matrix-data m)
                  (make-array (* (matrix-rows m) (matrix-cols m)) :element-type 'single-float))
            (dotimes (i (matrix-rows m) m)
              (dotimes (j (matrix-cols m))
                (setf (matrix-at m i j)
                      (funcall generator i j)))))
          (progn
            (setf (matrix-data m)
                  (make-array (* (matrix-rows m) (matrix-cols m))
                              :element-type 'single-float
                              :initial-element 0.0))
            m))))

(defun matrix-at (m i j)
  (aref (matrix-data m) (+ (* i (matrix-cols m)) j)))

(defun (setf matrix-at) (value m i j)
  (setf (aref (matrix-data m) (+ (* i (matrix-cols m)) j)) value))

(defun copy-matrix (m)
  (make-instance 'matrix
                 :rows (matrix-rows m)
                 :cols (matrix-cols m)
                 :data (copy-seq (matrix-data m))))

(defun transposed (m)
  (make-instance 'matrix
                 :rows (matrix-cols m)
                 :cols (matrix-rows m)
                 :generator #'(lambda (i j) (matrix-at m j i))))

(defun matrix-print (m)
  (dotimes (i (matrix-rows m) nil)
    (dotimes (j (matrix-cols m))
      (format t "~7,2f  " (matrix-at m i j)))
    (terpri)))

(defmacro do-matrix ((m i j &optional elt) &body body)
  `(dotimes (,i (matrix-rows ,m) ,m)
     (dotimes (,j (matrix-cols ,m))
       ,@(if elt
             `((symbol-macrolet ((,elt (matrix-data ,m ,i ,j)))
                 ,@body))
             body))))

(defmethod m* ((a matrix) (b matrix))
  (assert (= (matrix-cols a) (matrix-rows b)))
  (let ((result (make-instance 'matrix
                               :rows (matrix-rows a)
                               :cols (matrix-cols b))))
    (dotimes (i (matrix-rows result) result)
      (dotimes (j (matrix-cols result))
        (dotimes (k (matrix-cols a))
          (incf (matrix-at result i j)
                (* (matrix-at a i k) (matrix-at b k j))))))))
