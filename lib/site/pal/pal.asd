
(in-package #:asdf)

(defsystem pal
  :description "Pixel Art Library"
  :author "Tomi Neste"
  :license "MIT"
  :components
  ((:file "ffi"
          :depends-on ("package"))
   (:file "color"
          :depends-on ("package" "ffi"))
   (:file "vector"
          :depends-on ("pal-macros"))
   (:file "pal-macros"
          :depends-on ("ffi" "color"))
   (:file "pal"
          :depends-on ("pal-macros" "color" "vector"))
   (:file "package"))
  :depends-on ("cffi"))


