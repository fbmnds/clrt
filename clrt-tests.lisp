;;;;
;;;; clrt-tests.lisp
;;;;
(require 'asdf)
;(asdf:oos 'asdf:load-op 'clrt)
(ql:quickload 'clrt)

(load "linalg.lisp")
(load "camera.lisp")
(load "ray.lisp")
(load "objects")
(load "scene")

(defparameter *camera* (make-instance 'clrt-camera:camera
				      :pos (linalg:make-vector 3 :data (vector 0.0 0.0 0.0))
				      :up (linalg:make-vector 3 :data (vector 0.0 1.0 0.0))
				      :look-at (linalg:make-vector 3 :data (vector 0.0 0.0 100.0))))

(defparameter *scene* (make-instance 'clrt-scene:scene
				     :camera *camera*))

(clrt-scene:render *scene* 640 480 "test.png")

;; (defparameter *ray* (make-instance 'clrt-ray:ray
;; 				   :origin (linalg:make-vector 3 :data (vector 0.0 0.0 0.0))
;; 				   :direction (linalg:make-vector 3 :data (vector 0.0 0.0 1.0))))
;; 
;; (defparameter *sphere* (make-instance 'clrt-sphere:sphere
;; 				    :center (linalg:make-vector 3 :data (vector 0.0 0.0 100.0))
;; 				    :radius 20.0))
;; 
;; (format t "(intersects *sphere* *ray*) = ~d   .eq   1600.0" (clrt-sphere:intersects *sphere* *ray*))
;; (terpri)
	
