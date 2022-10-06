(in-package :cl-user)
(defpackage nail.linalg
  (:use :cl)
  (:export :matrix
           :matrix-at
           :|setf matrix-at|
           :matrix-row
           :matrix-col
           :copy-matrix
           :transposed
           :matrix-print
           :m*
           :mult
           :m+
           :m-
           :make-vector
           :vec-x
           :vec-y
           :vec-z
           :vec-length
           :normalized))


(in-package :nail.linalg)

(defmacro assert-same-dimensions (a b)
  `(assert (and (= (matrix-rows ,a) (matrix-rows ,b))
                (= (matrix-cols ,a) (matrix-cols ,b)))
           nil
           "The matrices ~a and ~a must have the same dimensions."
           ,a ,b))

(defmacro do-matrix ((m i j &optional elt) &body body)
  `(dotimes (,i (matrix-rows ,m) ,m)
     (dotimes (,j (matrix-cols ,m))
       ,@(if elt
             `((symbol-macrolet ((,elt (matrix-at ,m ,i ,j)))
                 ,@body))
             body))))

(defmacro def-elementwise-op-fun (name op)
  `(defun ,name (&rest matrices)
     (when (consp matrices)
       (let ((result (copy-matrix (car matrices))))
         (dolist (matrix (cdr matrices) result)
           (assert-same-dimensions result matrix)
           (map-into (matrix-data result) ,op
                     (matrix-data result)
                     (matrix-data matrix)))))))

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
                  (make-array (* (matrix-rows m) (matrix-cols m))
                              :element-type 'single-float))
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

(defun matrix-row (m i)
  (let ((result (make-array (matrix-cols m) :element-type 'single-float)))
    (dotimes (el (matrix-cols m) result)
      (setf (aref result el) (aref (matrix-data m) (+ el (* (matrix-cols m) i)))))))

(defun matrix-col (m j)
  (let ((result (make-array (matrix-rows m) :element-type 'single-float)))
    (dotimes (el (matrix-rows m) result)
      (setf (aref result el) (aref (matrix-data m) (+ j (* (matrix-cols m) el)))))))

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

(defgeneric m* (op1 op2))

(defmethod m* ((a matrix) (b matrix))
  (assert (= (matrix-cols a) (matrix-rows b)))
  (let ((result (make-instance 'matrix
                               :rows (matrix-rows a)
                               :cols (matrix-cols b))))
    (do-matrix (result i j elt)
      (dotimes (k (matrix-cols a))
        (incf elt (* (matrix-at a i k) (matrix-at b k j)))))
    (if (= 1 (matrix-cols result) (matrix-rows result))
        (aref (matrix-data result) 0)
        result)))

(defmethod m* ((m matrix) (s single-float))
  (make-instance 'matrix
                 :rows (matrix-rows m)
                 :cols (matrix-cols m)
                 :data (map 'vector #'(lambda (i) (* i s)) (matrix-data m))))

(defmethod m* ((s single-float) (m matrix))
  (m* m s))

(defmethod m* ((a single-float) (b single-float))
  (+ a b))

(defun mult (&rest operands)
  (when (consp operands)
    (let ((acc (car operands)))
      (dolist (operand (cdr operands) acc)
        (setf acc (m* acc operand))))))

(def-elementwise-op-fun m+ #'+)
(def-elementwise-op-fun m- #'-)

;; Vector works

(defun is-threedimensional-vector? (v)
  (or (and (= (matrix-rows v) 3)
           (= (matrix-cols v) 1))
      (and (= (matrix-rows v) 1)
           (= (matrix-cols v) 3))))

(defmacro make-vector (dim &key (orientation :column) data generator)
  (let ((i (gensym))
        (j (gensym)))
    (case orientation
      (:column
       `(make-instance 'matrix
                       :rows ,dim
                       :cols 1
                       ,@(if data
                             `(:data ,data)
                             (when generator
                               `(:generator #'(lambda (,i ,j) (funcall ,generator ,i)))))))
      (:row
       `(make-instance 'matrix
                       :rows 1
                       :cols ,dim
                       ,@(if data
                             `(:data ,data)
                             (when generator
                               `(:generator #'(lambda (,i ,j) (funcall ,generator ,j)))))))
      (t (error "Unknows :orientation '~A'" orientation)))))

(defun vec-x (v)
  (aref (matrix-data v) 0))

(defun vec-y (v)
  (aref (matrix-data v) 1))

(defun vec-z (v)
  (aref (matrix-data v) 2))

(defun vec-length (v)
  (sqrt (reduce #'(lambda (i j) (+ i (* j j))) (matrix-data v) :initial-value 0.0)))

(defun normalized (v)
  (let ((s (vec-length v)))
    (if (/= s 0)
        (make-instance 'matrix
                       :rows (matrix-rows v)
                       :cols (matrix-cols v)
                       :data (map 'vector #'(lambda (i) (/ i s)) (matrix-data v)))
        (copy-matrix v))))

(defun cross (a b)
  (assert (is-threedimensional-vector? a))
  (assert (is-threedimensional-vector? b))
  (make-instance 'matrix
                 :rows 3
                 :cols 1
                 :data (make-array 3 :element-type 'single-float
                                     :initial-contents
                                     (vector (- (* (vec-y a) (vec-z b))
                                                (* (vec-y b) (vec-z a)))
                                             (- (* (vec-z a) (vec-x b))
                                                (* (vec-z b) (vec-x a)))
                                             (- (* (vec-x a) (vec-y b))
                                                (* (vec-x b) (vec-y a)))))))
