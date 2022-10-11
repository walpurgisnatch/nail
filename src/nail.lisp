(in-package :cl-user)
(defpackage nail
  (:use :cl
        :nail.utils))

(in-package :nail)

(cl-reexport:reexport-from :nail.linalg)
(cl-reexport:reexport-from :nail.statistics)
