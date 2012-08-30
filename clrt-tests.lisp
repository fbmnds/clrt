;;;;
;;;; clrt-tests.lisp
;;;;

;;
;; use either:
;;
;; (require 'asdf)
;; (asdf:oos 'asdf:load-op 'clrt) ; old version (there is a newer one, ref. ASDF manual)
;;
;; or:
;;
(ql:quickload 'clrt)

;;
;; neither 'load' nor 'require' allows to direct use wihe pack
;;
(use-package 'linalg)
(use-package 'clrt-camera)
(use-package 'clrt-ray)
(use-package 'clrt-objects)
(use-package 'clrt-scene)

(defparameter *camera* (make-instance 'camera
				      :pos (make-vector 3 :data #(0.0 0.0 0.0))
				      :up (make-vector 3 :data #(0.0 1.0 0.0))
				      :look-at (make-vector 3 :data #(0.0 0.0 100.0))))

(defparameter *scene* (make-instance 'scene
				     :camera *camera*))

(render *scene* 640 480 "test.png")

(defparameter *ray* (make-instance 'ray
				   :origin (make-vector 3 :data #(0.0 0.0 0.0))
				   :direction (make-vector 3 :data #(0.0 0.0 1.0))))

(defparameter *sphere* (make-instance 'sphere
				    :center (make-vector 3 :data #(0.0 0.0 100.0))
				    :radius 20.0))

(format t "(intersects *sphere* *ray*) = ~d   .eq   1600.0" (intersects *sphere* *ray*))
(terpri)
	
