;;;;
;;;; camera.lisp
;;;;

(defpackage #:clrt-camera
  (:use :cl :linalg)
  (:export #:camera
	   #:camera-pos
	   #:camera-direction
	   #:camera-fov
	   #:world->view))

(in-package #:clrt-camera)

(defclass camera ()
  ((pos
   :initarg :pos
   :initform (error ":pos is mandatory")
   :type matrix
   :reader camera-pos)
  (dir
   :type matrix
   :reader camera-direction)
  (up
   :initarg :up
   :initform (error ":up is mandatory")
   :type matrix
   :reader camera-up)
  (fov
   :initarg :fov
   :initform 110.0
   :type (real 70.0 130.0)
   :reader camera-fov)
  (w2v-matrix
   :type matrix
   :accessor w2v-matrix)))


(defmethod initialize-instance :after ((cam camera) &key look-at)
  (setf (slot-value cam 'dir)
	(normalized (m- look-at (camera-pos cam)))))

(defmethod w2v-matrix :before ((cam camera))
  (unless (slot-boundp cam 'w2v-matrix)
    (let* ((right (normalized (cross (camera-up cam) (camera-direction cam))))
	   (up (normalized (cross (camera-direction cam) right)))
	   (dir (camera-direction cam))) ; should be normalized?
      (setf (w2v-matrix cam)
	    (make-instance 'matrix
			   :rows 3
			   :cols 3
			   :data (make-array 9
					     :element-type 'single-float
					     :initial-contents (vector (vec-x right)
								       (vec-y right)
								       (vec-z right)
								       (vec-x up)
								       (vec-y up)
								       (vec-z up)
								       (vec-x dir)
								       (vec-y dir)
								       (vec-z dir))))))))

(defun world->view (cam vec)
  (m* (w2v-matrix cam) vec))

