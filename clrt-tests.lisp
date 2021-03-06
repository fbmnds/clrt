;;;;
;;;; clrt-tests.lisp
;;;;

;;
;; use either:
;;
;; (require 'asdf)
;; (asdf:oos 'asdf:load-op 'clrt) ; old version (there is a newer one, ref. ASDF manual)
;;
;; or - better:
;;
(ql:quickload 'clrt)

(use-package 'linalg)
(use-package 'clrt-camera)
(use-package 'clrt-ray)
(use-package 'clrt-objects)
(use-package 'clrt-scene)
(use-package 'clrt-lights)
(use-package 'clrt-material)

(defparameter *camera* (make-instance 'camera
				      :pos (make-vector 3 :data #(50.0 50.0 0.0))
				      :up (make-vector 3 :data #(0.0 1.0 0.0))
				      :look-at (make-vector 3 :data #(0.0 0.0 100.0))))

(defparameter *light* (make-instance 'light
                                     :pos (make-vector 3 :data #(-100.0 100.0 0.0))))

(defparameter *scene-with-cube* (make-instance 'scene
				     :camera *camera*))

(defparameter *scene-with-sphere* (make-instance 'scene
				     :camera *camera*))

(defparameter *blue-material* (make-instance 'material
                                             :ambient-color (make-vector 3 :data #(0.0 0.0 0.3))
                                             :ambient-coeff 0.1
                                             :diffuse-color (make-vector 3 :data #(0.0 0.0 0.8))
                                             :diffuse-coeff 0.6
                                             :specular-color (make-vector 3 :data #(1.0 1.0 1.0))
                                             :specular-coeff 0.3
                                             :roughness 50))
                                             
(defparameter *cube* (make-instance 'cube
                                    :center (make-vector 3 :data #(0.0 0.0 100.0))
                                    :width 80.0
                                    :height 80.0
                                    :depth 80.0
                                    :material *blue-material*))

(finalize *cube* *camera*)
(add-object *scene-with-cube* *cube*)
(add-light *scene-with-cube* *light*)
(print "start rendering cube...")
(render *scene-with-cube* 640 480 "scene-with-cube.png")
(print "... finished.")

(defparameter *sphere* (make-instance 'sphere
				    :center (make-vector 3 :data #(0.0 0.0 80.0))
				    :radius 60.0
                                    :material *blue-material*))

(add-object *scene-with-sphere* *sphere*)
(add-light *scene-with-sphere* *light*)
(print "start rendering sphere...")
(render *scene-with-sphere* 640 480 "scene-with-sphere.png")
(print "... finished.")

(defparameter *ray* (make-instance 'ray
				   :origin (make-vector 3 :data #(0.0 0.0 0.0))
				   :direction (make-vector 3 :data #(0.0 0.0 1.0))))
(print (intersects *sphere* *ray*))
(print (intersects *cube* *ray*))

(defparameter *ray* (make-instance 'ray
				   :origin (make-vector 3 :data #(100.0 0.0 100.0))
				   :direction (make-vector 3 :data #(-1.0 0.0 0.0))))
(print (intersects *cube* *ray*))

(defparameter *ray* (make-instance 'ray
				   :origin (make-vector 3 :data #(0.0 100.0 100.0))
				   :direction (make-vector 3 :data #(0.0 -1.0 0.0))))
(print (intersects *cube* *ray*))

(in-package #:clrt-objects)
; .eq 1.0
(print (min-in-range '(1.0 2.0 3.0 4.0)))

; .eq 2.0
(print (min-in-range '(1.0 2.0 3.0 4.0) :lower-bound 2.0))

; .eq (1.0 . A)
(print (min-in-range '((1.0 . a) (2.0 . b) (3.0 . c) (4.0 . d)) :key #'car))

; .eq (2.0 . A)
(print (min-in-range '((1.0 . a) (2.0 . b) (3.0 . c) (4.0 . d)) :key #'car :lower-bound 2.0))
(in-package #:ccl)

;(format t "(intersects *sphere* *ray*) = ~d   .eq   1600.0" (intersects *sphere* *ray*))
(terpri)
(quit)

