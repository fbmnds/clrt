;;;;
;;;; material.lisp
;;;;

(defpackage #:clrt-material
  (:use :cl :linalg)
  (:export #:material
           #:ambient-color
           #:diffuse-color
           #:specular-color
           #:ambient-coeff
           #:diffuse-coeff
           #:specular-coeff
           #:roughness))

(in-package #:clrt-material)

(defclass material ()
  ((ambient-color
    :initarg :ambient-color
    :initform (make-vector 3) ; black
    :type matrix
    :reader ambient-color)
   (diffuse-color
    :initarg :diffuse-color
    :initform (make-vector 3 ; white
                           :data (make-array 3
                                             :element-type 'single-float
                                             :initial-value 1.0))
    :type matrix
    :reader diffuse-color)
   (specular-color
    :initarg :specular-color
    :initform (make-vector 3) ; black
    :type matrix
    :reader specular-color)
   (ambient-coeff
    :initarg :ambient-coeff
    :initform 0.0
    :type (real 0.0 1.0) ; intervall (0 1)
    :reader ambient-coeff)
   (diffuse-coeff
    :initarg :diffuse-coeff
    :initform 1.0
    :type (real 0.0 1.0) ; intervall (0 1)
    :reader diffuse-coeff)
   (specular-coeff
    :initarg :specular-coeff
    :initform 0.0
    :type (real 0.0 1.0) ; intervall (0 1)
    :reader specular-coeff)
   (roughness
    :initarg :roughness
    :initform 50.0
    :type (integer 0) ; >= 0
    :reader roughness)))

(defmethod initialize-instance :after ((mat material) &key)
  (assert (= (+ (ambient-coeff mat)
                (diffuse-coeff mat)
                (specular-coeff mat))
             1.0)
          nil
          ":ambient-coeff, :diffuse-coeff and :specular-coeff must sum up to 1.0"))

