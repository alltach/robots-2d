(require :asdf)

(defpackage robots.system
  (:use :cl :asdf))
(in-package :robots.system)

(defsystem "robots"
  :description "2d robot-game prototype"
  :author "alltach"
  :depends-on (:pal)
  :components ((:file "package")
               (:file "fw" :depends-on ("package"))
	       (:file "game" :depends-on ("fw" "package"))))
