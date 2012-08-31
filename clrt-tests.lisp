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

(defparameter *camera* (make-instance 'camera
				      :pos (make-vector 3 :data #(0.0 0.0 0.0))
				      :up (make-vector 3 :data #(0.0 1.0 0.0))
				      :look-at (make-vector 3 :data #(0.0 0.0 100.0))))

;(defparameter *scene* (make-instance 'scene
;				     :camera *camera*))

(defparameter *cube* (make-instance 'cube
                                    :center (make-vector 3 :data #(0.0 0.0 100.0))
                                    :width 20.0 :height 20.0 :depth 20.0))

(finalize *cube* *camera*)

(defparameter *ray* (make-instance 'ray
				   :origin (make-vector 3 :data #(0.0 0.0 0.0))
				   :direction (make-vector 3 :data #(0.0 0.0 1.0))))
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
(print (min-in-range '((1.0 .a) (2.0 . b) (3.0 . c) (4.0 . d)) :key #'car))

; .eq (2.0 . A)
(print (min-in-range '((1.0 .a) (2.0 . b) (3.0 . c) (4.0 . d)) :key #'car :lower-bound 2.0))
(in-package #:ccl)
;(defparameter *sphere* (make-instance 'sphere
;				    :center (make-vector 3 :data #(0.0 0.0 80.0))
;				    :radius 20.0))

;(add-object *scene* *sphere*)

;(render *scene* 640 480 "test.png")

;(format t "(intersects *sphere* *ray*) = ~d   .eq   1600.0" (intersects *sphere* *ray*))
(terpri)
(quit)

