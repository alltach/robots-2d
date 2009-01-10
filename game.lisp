
(in-package :robots) 

(defvar *resource-path* 
  (merge-pathnames "res/" (asdf:component-pathname 
			   (asdf:find-system :robots)))
  "The directory in which resource files are located")


(defun run ()
  "Run the game loop"
  (with-pal (:title "Robots" :paths (list *resource-path*))
    (let ((robot (load-static-sprite "robot.png")))
       (set-cursor nil)
       (event-loop ()
	 (clear-screen (color 232 123 44))   
	 (draw-fps)
         (draw-sprite robot)
	 (test-keys
	   (:key-w (move-direction robot  2))
	   (:key-s (move-direction robot -2))
	   (:key-a (rotate-by robot -2))
	   (:key-d (rotate-by robot +2)))))))
     
