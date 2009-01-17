(in-package :pal)

(declaim (optimize (speed 3)
                   (safety 1)))


(defstruct color
  (r 0 :type u8)
  (g 0 :type u8)
  (b 0 :type u8)
  (a 0 :type u8))


(declaim (inline color))
(defun color (r g b &optional (a 255))
  (declare (type u8 r) (type u8 g) (type u8 b) (type u8 a))
  (make-color :r r :g g :b b :a a))


(defun random-color ()
  (color (random 255) (random 255) (random 255) (random 255)))




(defparameter +black+ (color 0 0 0))
(defparameter +gray+ (color 128 128 128))
(defparameter +light-gray+ (color 200 200 200))
(defparameter +dark-gray+ (color 64 64 64))
(defparameter +white+ (color 255 255 255))
(defparameter +light-green+ (color 50 255 50))
(defparameter +light-blue+ (color 50 50 255))
(defparameter +red+ (color 255 20 20))