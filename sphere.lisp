;;;;
;;;; sphere.lisp
;;;;

(in-package #:clrt-objects)

(defclass sphere (object)
  ((radius
    :initarg :radius
    :initform (error ":radius is mandatory")
    :type single-float
    :reader sphere-radius)))
  
(defmethod intersects ((sphere sphere) (ray ray) &key (lower-bound 0.0) shadow-feeler)
  (let* ((ro (ray-origin ray))
	 (rd (ray-direction ray))
	 (c (object-center sphere))
	 (r (sphere-radius sphere))
	 (ro*ro (dot ro ro))
	 (ro*rd (dot ro rd))
	 (ro*c (dot ro c))
	 (rd*rd (dot rd rd))
	 (c*c (dot c c))
	 (c*rd (dot c rd))
	 (discr (- (* 4 (expt (- ro*rd c*rd) 2))
		   (* 4 rd*rd (- ro*ro (* 2 ro*c) (- c*c) (* r r)))))
         (tmin (min-in-range (cond
                              ((< discr 0) nil)
                              ((= discr 0) (list (/ (* -2 (- ro*rd c*rd)) (* 2 rd*rd))))
                              (t (let ((root (sqrt discr)))
                               (list (/ (* -2 (- ro*rd c*rd)) (* 2 rd*rd))
                                                 (/ (* -2 (- ro*rd c*rd)) (* 2 rd*rd))))))
               :lower-bound lower-bound
               :upper-bound shadow-feeler)))
  (when tmin
    (list tmin sphere (point-on-ray ray tmin)))))

