(in-package :robots)

;;; Generic sprite

(defclass sprite () 
  ((drawn :reader sprite-drawn-p
          :writer (setf drawn)
          :type boolean
          :initarg :drawn
          :initform T
	  :documentation "Whether the sprite is shown or hidden")
   (pos :accessor sprite-pos
	:type vec
	:initarg :pos
	:initform (v 0 0)
	:documentation "The sprite's position")
   (vel :accessor sprite-vel
	:type vec
	:initarg :vel
	:initform (v 0 0)
	:documentation "The sprite's velocity")
   (angle :accessor sprite-angle
	  :type integer
	  :initarg angle
	  :initform 0
	  :documentation "The sprite's rotation"))
  (:documentation "The abstract base class for all sprites"))

(defgeneric draw-sprite (sprite)
  (:documentation "Draw the sprite on screen"))

(defmethod move-by ((sprite sprite) (pos vec))
  "Change the sprite's position by x and y"
  (v+! (sprite-pos sprite) pos))  

(defmethod move-to ((sprite sprite) (pos vec))
  "Set the sprite's position to x y"
  (setf (sprite-pos sprite) pos))

(defmethod move-direction ((sprite sprite) units)
  "Move the sprite in forward/backwards according to it's current angle"
  (with-slots (angle) sprite
    (move-by sprite (v* (angle-v angle) units))))
  
(defmethod rotate-by ((sprite sprite) ang)
  "Change the sprite's angle by ang"
  (with-slots (angle) sprite
    (setf angle (mod (+ angle ang) 360))))

(defmethod rotate-to ((sprite sprite) ang)
  "Set the sprite's angle to ang"
  (setf (sprite-angle sprite) (mod ang 360)))


;;; Static sprite

(defclass static-sprite (sprite)
  ((path :accessor sprite-image-path
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
  "Unload/free the sprite image."
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
    "Create a static sprite and load it.
     Return nil if not loaded"
    (load-sprite 
     (make-instance 'static-sprite :path path)))


;;; Utils

(defmacro aif (c th &optional el)
  `(let ((it ,c))
     (if it ,th ,el)))
