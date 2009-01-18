(in-package :robots)
(declaim (optimize (debug 1)))

; ------------------------------------------------------------------
;                                                                  |
; TODO: 
;    - collision 
;    - real scene graph (not just a flat list)
;    - convenience macros for game, sprite...
;    - context-sensitive functions
;      for use within event callbacks (e.g. set-focus)
;    - animation, timing
;    - background tiling stuff and animated sprites (in PAL)
;                                                                  |
; ------------------------------------------------------------------


;;; Event

(defclass event ()
  ((time :reader event-time
	 :type integer
	 :initarg :time
	 :documentation "The event start time"))
  (:documentation "Base class for events"))

(defclass tick-event (event) ()
  (:documentation "Event on every game tick"))

(defclass key-event (event)
  ((key :reader key-event-key
	:type symbol
	:initarg :key
	:documentation "The key that changed state"))
  (:documentation "Event on key state change"))

(defclass key-up-event (key-event) ()
  (:documentation "Event on key release")) 

(defclass key-down-event (key-event) ()
  (:documentation "Event on key press"))

(defclass mouse-event (event) 
  ((pos :reader mouse-event-pos
	:type vec
	:initarg :pos
	:documentation "The mouse position"))
  (:documentation "Event on mouse state change"))

(defclass mouse-move-event (mouse-event) ()
  (:documentation "Event on mouse move"))

(defclass mouse-button-event (mouse-event)
  ((button :reader mouse-event-button
	   :type integer
	   :initarg :button
	   :documentation "The mouse button that changed state"))
  (:documentation "Event on mouse button state change"))

(defclass mouse-button-up-event (mouse-button-event) ()
  (:documentation "Event on mouse button release"))

(defclass mouse-button-down-event (mouse-button-event) ()
  (:documentation "Event on mouse button press"))


;;; Event receiver

(defclass event-receiver () ()
  (:documentation "Mixin for game objects capable of handling events"))

(defgeneric handle-event (receiver event)
  (:documentation "Handle a single game event"))

(defmethod handle-event ((rec event-receiver) (ev event))
  nil)


;;; Game

(defclass game (event-receiver)
  ((focus :accessor game-focus
	  :type (or nil event-receiver)
	  :initarg :focus
	  :initform nil
	  :documentation "The currently focused game-object or nil")  
   (scene :accessor game-scene
	  :type list
	  :initarg :scene
	  :initform ()
	  :documentation "The scene graph")
   (default-handler :accessor game-default-handler
                    :type (or nil function)
		    :initarg :default-handler
		    :initform nil
		    :documentation "The default event handler"))
  (:documentation "Class for games, receives events if nothing is focused"))
	 
(defmethod handle-event ((game game) (event event))
  (with-slots (focus default-handler) game
      (if focus
	  (handle-event focus event)
	  (funcall default-handler game event))))

(defmethod run-game ((game game) &key (title "game") (width 640) (height 480) (fps 30) (paths ()))
  "Run the game: Fork off an event-handling/time-counting thread and wait for
events to appear on the event queue. Let the game handle them."
  (let ((evtq-mutex (make-mutex :name "event queue mutex"))
	(evtq (make-queue)))
    (with-pal (:title title :width width :height height :fps fps :paths paths)
      (let ((event-thread
	     (fork-function 
	      #'(lambda ()
		  (macrolet ((eventfn (event-type-form lambda-list &rest other-keys)
			       `(lambda ,lambda-list
				  (with-mutex (evtq-mutex)
				    (enqueue (make-instance ,event-type-form
							    :time (get-internal-real-time) 
							    ,@other-keys)
					     evtq)))))
		    (pal:event-loop (:key-up-fn (eventfn 'key-up-event (key) :key key)
						:key-down-fn (eventfn 'key-down-event (key) :key key)
						:mouse-btn-up-fn (eventfn 'mouse-button-up-event (btn pos) :button btn :pos pos)
						:mouse-btn-down-fn (eventfn 'mouse-button-down-event (btn pos) :button btn :pos pos)
						:mouse-motion-fn (eventfn 'mouse-move-event (x y) :pos (v x y)))
		      (thread-yield)))))))
	(unwind-protect (loop 
			   (aif (with-mutex (evtq-mutex) 
				  (if (empty-p evtq)
				      nil
				      (dequeue evtq)))
				(handle-event game it)))
	  (thread-interrupt event-thread #'thread-exit))))))
	

;;; Generic sprite

(defclass sprite (event-receiver) 
  ((drawn :reader sprite-drawn-p
          :writer (setf drawn)
          :type boolean
          :initarg :drawn
          :initform T
	  :documentation "Whether the sprite is shown or hidden")
   (pos   :accessor sprite-pos
	  :type vec
	  :initarg :pos
	  :initform (v 0 0)
	  :documentation "The sprite's position")
   (vel   :accessor sprite-vel
	  :type vec
	  :initarg :vel
	  :initform (v 0 0)
	  :documentation "The sprite's velocity")
   (angle :reader sprite-angle
       ;; :writer rotate-to --- is implemented as method 	  
	  :type (integer 0 360)
	  :initarg angle
	  :initform 0
	  :documentation "The sprite's rotation"))
  (:documentation "Base class for all sprites"))

(defgeneric draw-sprite (sprite)
  (:documentation "Draw the sprite on screen"))

(defmethod move-to ((sprite sprite) (pos vec))
  "Set the sprite's position to pos"
  (setf (slot-value sprite 'pos) pos)) 

(defmethod move-by ((sprite sprite) (pos vec))
  "Change the sprite's position by x and y"
  (v+! (slot-value sprite 'pos) pos))  

(defmethod move-direction ((sprite sprite) units)
  "Move the sprite in forward/backwards according to it's current angle"
  (with-slots (angle) sprite
    (move-by sprite (v* (angle-v angle) units))))
  
(defmethod rotate-by ((sprite sprite) ang)
  "Change the sprite's angle by ang"
  (with-slots (angle) sprite
    (setf angle 
	  (mod (+ angle (the integer ang)) 360))))

(defmethod rotate-to ((sprite sprite) ang)
  "Set the sprite's angle to ang"
  (setf (slot-value sprite 'angle) 
	(mod (the integer ang) 360)))


;;; Static sprite

(defclass static-sprite (sprite)
  ((path  :accessor sprite-image-path
	  :type (or pathname string)
	  :initarg :path)
   (image :type (or nil image)
	  :initform nil
          :reader sprite-loaded-p))
  (:documentation "The class for sprites consisting of a static image"))

(defmethod load-sprite ((s static-sprite))
  "Load the sprite image into memory"
  (with-slots (image path) s 
    (setf image (load-image path))
    s))

(defmethod unload-sprite ((s static-sprite))
  "Unload/free the sprite image"
  (with-slots (image) s
    (pal-ffi:free-resource image)
    (setf image nil)
    s))
  
(defmethod draw-sprite ((s static-sprite))
  (with-slots (drawn image pos angle) s
    (if (and drawn image)
	(draw-image image pos 
		    :angle angle
		    :valign :middle
		    :halign :middle))))

(defun load-static-sprite (path)
    "Create a static sprite and load it"
    (load-sprite 
     (make-instance 'static-sprite :path path)))


;;; Generated Sprite

(defclass generated-sprite (sprite)
  ((function :accessor sprite-function
	     :type function
	     :initarg :function
	     :documentation "Function drawing the sprite's representation")))

(defmethod draw-sprite ((sprite generated-sprite))
  (with-slots (drawn function pos angle) sprite
      (if drawn (funcall function pos angle))))


