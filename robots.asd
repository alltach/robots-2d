(require :asdf)

(defpackage robots.system
  (:use :cl :asdf))
(in-package :robots.system)

(pushnew (merge-pathnames
	  #P"lib/systems/"
	  (make-pathname :name nil 
			 :type nil 
			 :version nil
			 :defaults
			 (truename (asdf:system-definition-pathname '#:robots))))
	 asdf:*central-registry*)

(defsystem "robots"
  :description "2d robot-game prototype"
  :author "alltach"
  :depends-on (:pal)
  :components ((:module "fw"
			:depends-on ("package")
			:components ((:file "fw" :depends-on ("utils"))
				     (:file "utils")))
	       (:file "package")
	       (:file "game" :depends-on ("package" "fw"))))

