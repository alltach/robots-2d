(in-package :robots) 

(defvar *resource-path* 
  (merge-pathnames "res/" (asdf:component-pathname 
			   (asdf:find-system :robots)))
  "The directory in which resource files are located")

(defun draw-turtle (pos ang)
  (declare (ignore ang))
  (draw-line pos (v 10 10) 0 0 255 255))

;; (defun run ()
;;   "Run the game loop"
;;   (let ((bg (color 223 123 44)))
;;     (with-pal (:title "Robots" :paths (list *resource-path*))
;;       (let ((turtle (make-instance 'generated-sprite 
;; 				   :pos (v 10 10) 
;; 				   :function #'draw-turtle)))
;; 	(set-cursor nil)
;; 	(clear-screen bg)   
;; 	(event-loop ()
;; 	  (draw-sprite turtle)
;; 	  (test-keys
;; 	    (:key-w (move-direction turtle  4))
;; 	    (:key-s (move-direction turtle -4))
;; 	    (:key-a (rotate-by turtle -2))
;; 	    (:key-d (rotate-by turtle +2))
;; 	    (:key-space (progn (clear-screen bg)
;; 			       (move-to turtle (v 10 10))
;; 			       (rotate-to turtle 0)))
;; 	    (:key-escape (return-from event-loop))))))))

(defun handle-it (game evt)
  (declare (ignore game))
  (typecase evt
     (key-down-event (if (eq (key-event-key evt) :key-space)
			 (sb-ext:quit)
			 (format t "~a :: ~a~%" (event-time evt) (key-event-key evt))))
     (mouse-button-down-event (format t "~a :: button ~a @ (~a,~a)~%" (event-time evt) (mouse-event-button evt)
				      (vx (mouse-event-pos evt))
				      (vy (mouse-event-pos evt))))
     (mouse-move-event (format t "~a :: (~a,~a)~%" (event-time evt) (vx (mouse-event-pos evt)) (vy (mouse-event-pos evt))))))
  
(defun run ()
  (let ((game (make-instance 'game :default-handler #'handle-it)))
    (run-game game :paths (list *resource-path*))))