;;;;
;;;; objects.lisp
;;;;

(defpackage #:clrt-objects
  (:use #:cl #:linalg #:clrt-camera #:clrt-ray)
  (:export #:object
	   #:object-material
	   #:intersects
	   #:finalize
           #:sphere
           #:cube))

(in-package #:clrt-objects)

(defclass object ()
  ((center
   :initarg :center
   :initform (error ":center is mandatory")
   :type matrix
   :reader object-center)
  (material
   :reader object-material)))

(defgeneric intersects (obj ray &key lower-bound shadow-feeler))

(defgeneric finalize (obj cam))

(defmethod finalize ((obj object) (cam camera))
  (setf (slot-value obj 'center)
	(world->view cam (slot-value obj 'center))))

(defun min-in-range (elements &key (lower-bound 0.0) upper-bound (key #'identity))
  (let ((elts (remove-if-not #'(lambda (i)
                                 (if upper-bound
                                     (<= lower-bound i upper-bound)
                                   (<= lower-bound i)))
                             elements :key key)))
    (when elts
      (reduce #'(lambda (a b) (if (<= (funcall key a) (funcall key b))
                                  a
                                b))
              elts))))

(defun intersects-face (origin up right ray test-fn)
  (let* ((a (vec-x right))
         (b (vec-x up))
         (c (- (vec-x (ray-direction ray))))
         (d (vec-y right))
         (e (vec-y up))
         (f (- (vec-y (ray-direction ray))))
         (g (vec-z right))
         (h (vec-z up))
         (i (- (vec-z (ray-direction ray))))
         (det (- (+ (* a e i) (* b f g) (* c d h))
                 (+ (* c e g) (* b d i) (* a f h)))))
    (when (/= det 0)
      (let* ((rhs (m- (ray-origin ray) origin))
             (u (/ (+ (* (- (* e i) (* f h)) (vec-x rhs))
                      (* (- (* c h) (* b i)) (vec-y rhs))
                      (* (- (* b f) (* c e)) (vec-z rhs)))
                   det))
             (v (/ (+ (* (- (* f g) (* d i)) (vec-x rhs))
                      (* (- (* a i) (* c g)) (vec-y rhs))
                      (* (- (* c d) (* a f)) (vec-z rhs)))
                   det))
             (dist (/ (+ (* (- (* d h) (* e g)) (vec-x rhs))
                         (* (- (* b g) (* a h)) (vec-y rhs))
                         (* (- (* a e) (* b d)) (vec-z rhs)))
                      det)))
        (when (funcall test-fn u v)
          (values dist u v))))))

                  