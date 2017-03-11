;;;; universe-2d.asd

(asdf:defsystem #:universe-2d
  :description "Describe universe-2d here"
  :author "Antonis Kalou <kalouantonis@gmail.com>"
  :license "BSDv3"
  :depends-on (#:alexandria
               #:sdl2
               #:rtg-math
               #:cl-graph)
  :serial t
  :components ((:file "package")
               (:file "universe-2d"
                :depends-on ("package" "utils" "math"))
               (:file "math" :depends-on ("package"))
               (:file "utils" :depends-on ("package"))))

