;;;;
;;;; sphere.lisp
;;;;

(in-package #:clrt-objects)

(export 'sphere)

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
		   (* 4 rd*rd (- ro*ro (* 2 ro*c) (- c*c) (* r r))))))
    (print discr)))

