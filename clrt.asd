;;;;
;;;; clrt.asd
;;;;

(require 'asdf)

(asdf:defsystem :clrt
    :description "clrt - a simple lisp ray tracer"
    :depends-on (:zpng)
    :components
    ((:file "linalg")
     (:file "camera" :depends-on ("linalg"))
     (:file "objects" :depends-on ("linalg" "camera" "ray"))
     (:file "scene" :depends-on ("linalg" "camera" "ray" "objects"))
     (:file "sphere" :depends-on ("linalg" "objects"))
     (:file "ray" :depends-on ("linalg"))
     (:file "cube":depends-on ("linalg" "objects"))))
;     (:file "clrt-tests" :depends-on ("linalg" "camera" "objects" "ray" "scene" "sphere"))))



