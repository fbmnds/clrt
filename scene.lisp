;;;;
;;;; scene.lisp
;;;;

(defpackage :clrt-scene
  (:use :cl :linalg :clrt-camera :clrt-objects)
  (:export #:scene
	   #:add-object
	   #:render))

(ql:quickload "zpng")

(in-package :clrt-scene)

(defclass scene ()
  ((camera
    :initarg :camera
    :initform (error ":camera is mandatory")
    :type camera
    :reader scene-camera)
   (objects
    :initform '()
    :reader scene-objects)
   (lights
    :initform '()
    :reader scene-lights)))

(defun add-object (scene object)
  (push object (slot-value scene 'objects)))

(defun render (scene width height filename)
  (let* ((image (make-instance 'zpng:png
			       :width width
			       :height height))
	 (image-data (zpng:data-array image))
	 (delta (* pi (/ (camera-fov (scene-camera scene)) 360.0)))
	 (maxx (coerce (tan delta) 'single-float))
	 (minx (- maxx))
	 (maxy (* maxx (/ (coerce height 'single-float) (coerce width 'single-float))))
	 (stepx (/ (* 2.0 maxx) (coerce width 'single-float)))
	 (stepy (/ (* 2.0 maxy) (coerce height 'single-float))))
    (do ((y 0 (1+ y ))
        (y-coord maxy (- y-coord stepy)))
	((>= y height))
      (do* ((x 0 (1+ x))
	    (x-coord minx (+ x-coord stepx))
	    (ray-dir (make-vector 3 :data (make-array 3
						      :element-type 'single-float
						      :initial-contents (vector x-coord y-coord 1.0)))))
	  ((>= x width))
	(setf (aref image-data (truncate y) (truncate x) 1) 255)))
    (zpng:write-png image filename :if-exists :supersede)))


