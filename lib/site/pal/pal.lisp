
(declaim (optimize (speed 3)
                   (safety 1)))

(in-package :pal)

(defparameter *pal-directory* (make-pathname :directory (pathname-directory *load-pathname*)
                                             :host (pathname-host *load-pathname*)
                                             :device (pathname-device *load-pathname*)))

(defvar *messages* nil "List of messages draw on screen with MESSAGE.")
(defvar *pal-running* nil "T if PAL is already running.")
(defvar *title* "" "PAL windows title. Also used for creating the path to applications data directory.")
(defvar *ticks* 0)
(defvar *clip-stack* nil)
(defvar *fps* 0)
(defvar *new-fps* 0)
(defvar *delay* 0)
(defvar *max-fps* 0)
(defvar *data-paths* nil)
(defvar *pressed-keys* (make-hash-table :test #'eq :size 5))
(defvar *width* 0)
(defvar *height* 0)
(defvar *cursor* nil)
(defvar *cursor-offset* (v 0 0))
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *current-image* nil "Currently set OpenGL texture.")
(defvar *max-texture-size* 0 "Maximum size of OpenGL texture supported by system.")
(defvar *quads-open* nil "T if (GL-BEGIN +GL-QUADS+) is already in effect.")


(declaim (type list *messages*)
         (type list *clip-stack*)
         (type hash-table *pressed-keys*)
         (type u16 *mouse-x*)
         (type u16 *mouse-y*)
         (type u16 *width*)
         (type u16 *height*)
         (type vec *cursor-offset*)
         (type integer *ticks*)
         (type fixnum *new-fps*)
         (type fixnum *fps*)
         (type u11 *max-fps*)
         (type u11 *delay*)
         (type boolean *quads-open*)
         (type (or boolean image) *cursor*)
         (type (or boolean image) *current-image*))


(defun open-pal (&key (width 800) (height 600) (fps 60) (title "PAL") (fullscreenp nil) (paths nil))
  (when *pal-running*
    (close-pal))
  (pal-ffi:init (logior pal-ffi:+init-video+ pal-ffi:+init-audio+))
  (pal-ffi:open-audio 22050 pal-ffi:+audio-s16+ 0 1024)
  (pal-ffi:gl-set-attribute pal-ffi:+gl-depth-size+ 0)
  (pal-ffi:gl-set-attribute pal-ffi:+gl-doublebuffer+ 1)
  (let ((surface (pal-ffi::set-video-mode
                  width
                  height
                  0
                  (logior (if fullscreenp
                              pal-ffi::+fullscreen+
                              0)
                          pal-ffi:+opengl+))))
    (when (cffi:null-pointer-p surface)
      (error "PAL failed to obtain SDL surface"))
    (setf *data-paths* nil
          *max-texture-size* (pal-ffi:gl-get-integer pal-ffi:+gl-max-texture-size+)
          *messages* nil
          *pressed-keys* (make-hash-table :test 'eq)
          *ticks* (get-internal-real-time)
          *title* title
          *current-image* nil
          *max-fps* (truncate 1000 fps)
          *ticks* (pal-ffi:get-tick)
          *clip-stack* nil
          *quads-open* nil
          *fps* 1
          *delay* 0
          *new-fps* 0
          *cursor* t
          *cursor-offset* (v 0 0)
          *pal-running* t
          *width* (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:w)
          *height* (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:h))
    (pal-ffi:set-caption title (cffi:null-pointer))
    (pal-ffi:gl-disable pal-ffi:+gl-cull-face-test+)
    (pal-ffi:gl-enable pal-ffi:+gl-texture-2d+)
    (pal-ffi:gl-shade-model pal-ffi:+gl-flat+)
    (pal-ffi:gl-disable pal-ffi:+gl-scissor-test+)
    (set-blend-mode :blend)
    (pal-ffi:gl-viewport 0 0 *width* *height*)
    (pal-ffi:gl-matrix-mode pal-ffi:+gl-projection+)
    (pal-ffi:gl-load-identity)
    (pal-ffi:gl-ortho 0d0 (coerce *width* 'double-float) (coerce *height* 'double-float) 0d0 -1d0 1d0)
    (pal-ffi:gl-matrix-mode pal-ffi:+gl-modelview+)
    (pal-ffi:gl-load-identity)
    (pal-ffi:gl-pixel-store pal-ffi:+gl-pack-alignment+ 1)
    (clear-screen +black+)
    (reset-tags)
    (define-tags default-font (load-font "default-font"))
    (add-path *pal-directory*)
    (add-path *default-pathname-defaults*)
    (if (listp paths)
        (dolist (p paths)
          (add-path p))
        (add-path paths))))

(declaim (inline clamp))
(defun clamp (min v max)
  (declare (number min max))
  (max min (min max v)))

(defun random-elt (sequence)
  (elt sequence (random (length sequence))))

(defun free-resource (resource)
  (close-quads)
  (pal-ffi:free-resource resource))

(defun free-all-resources ()
  (reset-tags)
  (pal-ffi:halt-music)
  (pal-ffi:halt-channel -1)
  (unless (eq *cursor* t)
    (set-cursor nil))
  (pal-ffi:free-all-resources))

(defun close-pal ()
  (unwind-protect
       (progn (free-all-resources)
              (pal-ffi:close-audio)
              (pal-ffi:show-cursor t))
    (pal-ffi:quit)
    (setf *pal-running* nil)))

(defun get-application-folder ()
  "Return the application data directory to be used for saving user specific data. PAL windows title is used when forming the directory pathname. Actual behaviour depends on the operating system."
  (assert (> (length *title* ) 0))
  #-win32 (ensure-directories-exist (merge-pathnames (make-pathname :directory (list :relative (concatenate 'string "." *title*)))
                                                     (user-homedir-pathname)))
  #+win32 (ensure-directories-exist (merge-pathnames (make-pathname :directory (list :relative *title*))
                                                     (parse-namestring (pal-ffi:get-application-folder)))))

(defun get-application-file (file)
  "Return a full path to a FILE in the application data directory. PAL windows title is used when forming the directory pathname. Actual behaviour depends on the operating system."
  (merge-pathnames file (get-application-folder)))

(defun add-path (path)
  "Add PATH to the list of paths that are searched when loading resources."
  (if #-:clisp (probe-file path)
      #+:clisp (ext:probe-directory path)
      (pushnew path *data-paths*)
      (format *debug-io* "Illegal data path: ~a" path)))

(defun data-path (file)
  "Find a FILE from the search paths."
  (let ((result nil))
    (dolist (i *data-paths* result)
      (let ((truename (probe-file (merge-pathnames file i))))
        (when truename
          (setf result (namestring (merge-pathnames truename i))))))
    (if result
        result
        (error "Data file not found: ~a" file))))

(defun get-gl-info ()
  "Return some information about systems OpenGL implementation."
  (list :vendor (pal-ffi:gl-get-string pal-ffi:+gl-vendor+)
        :rendered (pal-ffi:gl-get-string pal-ffi:+gl-renderer+)
        :version (pal-ffi:gl-get-string pal-ffi:+gl-version+)
        :extensions (pal-ffi:gl-get-string pal-ffi:+gl-extensions+)
        :max-texture-size *max-texture-size*))



;; Events

(declaim (inline key-pressed-p))
(defunct key-pressed-p (keysym)
    (symbol keysym)
  "Return T if key KEYSYM is currently pressed down."
  (gethash keysym *pressed-keys*))

(defunct keysym-char (keysym)
    (symbol keysym)
  (if (or (eq keysym :key-mouse-1) (eq keysym :key-mouse-2) (eq keysym :key-mouse-3) (eq keysym :key-mouse-4) (eq keysym :key-mouse-5))
      nil
      (let ((kv (cffi:foreign-enum-value 'pal-ffi:sdl-key keysym)))
        (if (and (> kv 0) (< kv 256))
            (code-char kv)
            nil))))

(declaim (inline get-mouse-pos))
(defun get-mouse-pos ()
  (v *mouse-x* *mouse-y*))

(declaim (inline get-mouse-x))
(defun get-mouse-x ()
  *mouse-x*)

(declaim (inline get-mouse-y))
(defun get-mouse-y ()
  *mouse-y*)

(defun handle-events (&key key-up-fn key-down-fn mouse-btn-up-fn mouse-btn-down-fn mouse-motion-fn quit-fn)
  (block event-loop
    (cffi:with-foreign-object (event :char 500)
      (do-event event key-up-fn key-down-fn mouse-btn-up-fn mouse-btn-down-fn mouse-motion-fn quit-fn))))

(defun wait-keypress ()
  "Wait until some key is pressed down and released."
  (let ((key nil))
    (event-loop (:key-down-fn (lambda (k)
                                (setf key k)
                                (return-from event-loop key))))
    (event-loop (:key-up-fn (lambda (k)
                              (when (eq key k)
                                (return-from event-loop key)))))
    key))



;; Screen

(defun draw-messages ()
  (let ((fh (get-font-height))
        (y 0))
    (declare (type u11 y fh))
    (dolist (m *messages*)
      (declare (type simple-string m))
      (draw-text m (v 0 (incf y fh))))))

(defun update-screen ()
  "Updates PAL window."
  (setf *new-fps* (max 1 (the fixnum (- (pal-ffi:get-tick) *ticks*))))
  (setf *fps* (truncate (+ *fps* *new-fps*) 2))
  (if (> *delay* 0)
      (decf *delay*))
  (when (< *fps* *max-fps*)
    (incf *delay* (- *max-fps* *fps*)))
  (setf *ticks* (pal-ffi:get-tick))
  (pal-ffi:delay *delay*)
  (if (or (eq t *cursor*) (eq nil *cursor*))
      (when *messages*
        (with-default-settings
          (draw-messages)))
      (with-default-settings
        (draw-image *cursor* (v- (get-mouse-pos) *cursor-offset*))
        (draw-messages)))
  (close-quads)
  (pal-ffi:gl-swap-buffers)
  (let ((e (pal-ffi:gl-get-error)))
    (unless (= e 0)
      (error "GL error ~a" e))))

(declaim (inline get-screen-width))
(defun get-screen-width ()
  *width*)

(declaim (inline get-screen-height))
(defun get-screen-height ()
  *height*)

(declaim (inline get-fps))
(defun get-fps ()
  (truncate 1000 *fps*))

(declaim (inline clear-screen))
(defunct clear-screen (color)
    (color color)
  (close-quads)
  (pal-ffi:gl-clear-color (/ (color-r color) 255f0)
                          (/ (color-g color) 255f0)
                          (/ (color-b color) 255f0)
                          1f0)
  (pal-ffi:gl-clear pal-ffi:+gl-color-buffer-bit+))

(defunct set-mouse-pos (x y)
    (u16 x u16 y)
  (pal-ffi:warp-mouse x y)
  (setf *mouse-x* x
        *mouse-y* y))

(defun set-cursor (image &optional offset)
  "Sets the state of mouse cursor. When IMAGE is NIL hide the cursor, when T show it. If IMAGE is an image resource use that as mouse cursor. OFFSET is a vector that sets the offset of custom cursor image."
  (assert (and (or (null offset) (vec-p offset))
               (or (image-p image) (typep image 'boolean))))
  (when offset
    (setf *cursor-offset* offset))
  (cond
    ((eq image t)
     (pal-ffi:show-cursor t))
    ((eq image nil)
     (pal-ffi:show-cursor nil))
    ((image-p image)
     (setf *cursor* image)
     (pal-ffi:show-cursor nil)))
  image)

(defunct push-clip (x y width height)
    (u16 x u16 y u16 width u16 height)
  (close-quads)
  (let ((y (- (get-screen-height) y height)))
    (pal-ffi:gl-scissor x y width height)
    (pal-ffi:gl-enable pal-ffi:+gl-scissor-test+)
    (push (vector x y width height) *clip-stack*)))

(defun pop-clip ()
  (close-quads)
  (pop *clip-stack*)
  (if *clip-stack*
      (let ((r (first *clip-stack*)))
        (pal-ffi:gl-scissor (aref r 0) (aref r 1) (aref r 2) (aref r 3)))
      (pal-ffi:gl-disable pal-ffi:+gl-scissor-test+)))




;; State


(declaim (inline open-quads))
(defun open-quads ()
  (unless *quads-open*
    (pal-ffi:gl-begin pal-ffi:+gl-quads+)
    (setf *quads-open* t)))

(declaim (inline close-quads))
(defun close-quads ()
  (when *quads-open*
    (pal-ffi:gl-end)
    (setf *quads-open* nil)))

(declaim (inline set-blend-mode))
(defunct set-blend-mode (mode)
    (symbol mode)
  (close-quads)
  (case mode
    ((nil) (pal-ffi:gl-disable pal-ffi:+gl-blend+))
    (:blend (pal-ffi:gl-enable pal-ffi:+gl-blend+)
            (pal-ffi:gl-blendfunc pal-ffi:+gl-src-alpha+ pal-ffi:+gl-one-minus-src-alpha+))
    (:additive (pal-ffi:gl-enable pal-ffi:+gl-blend+)
               (pal-ffi:gl-blendfunc pal-ffi:+gl-src-alpha+ pal-ffi:+gl-one+))))

(declaim (inline rotate))
(defunct rotate (angle)
    (single-float angle)
  (close-quads)
  (pal-ffi:gl-rotatef angle 0f0 0f0 1f0))

(declaim (inline scale))
(defunct scale (x y)
    (single-float x single-float y)
  (close-quads)
  (pal-ffi:gl-scalef x y 1f0))

(declaim (inline translate))
(defunct translate (vec)
    (vec vec)
  (close-quads)
  (pal-ffi:gl-translatef (vx vec) (vy vec) 0f0))

(declaim (inline reset-blend))
(defun reset-blend ()
  (close-quads)
  (set-blend-mode :blend)
  (set-blend-color +white+))

(declaim (inline set-blend-color))
(defunct set-blend-color (color)
    (color color)
  (pal-ffi:gl-color4ub (color-r color) (color-g color) (color-b color) (color-a color)))

(declaim (inline set-image))
(defunct set-image (image)
    (image image)
  (unless (eq image *current-image*)
    (close-quads)
    (setf *current-image* image)
    (pal-ffi:gl-bind-texture pal-ffi:+gl-texture-2d+ (pal-ffi::image-texture image))))



;; Images

(defun surface-get-pixel (image x y)
  (declare (type u11 x y))
  (let* ((bpp (cffi:foreign-slot-value (cffi:foreign-slot-value image 'pal-ffi:surface 'pal-ffi:pixelformat) 'pal-ffi:pixelformat 'pal-ffi:bytesperpixel))
         (pixels (cffi:foreign-slot-value image 'pal-ffi:surface 'pal-ffi:pixels))
         (pitch (cffi:foreign-slot-value image 'pal-ffi:surface 'pal-ffi:pitch))
         (point (+ (* y pitch) (* x bpp)))
         (pixel (case bpp
                  (1 (cffi:mem-ref pixels :uint8 point))
                  (2 (cffi:mem-ref pixels :uint16 point))
                  (3 (logior (logior (logior (cffi:mem-ref pixels :uint8 point)
                                             (ash (cffi:mem-ref pixels :uint8 (+ point 1)) 8)))
                             (ash (cffi:mem-ref pixels :uint8 (+ point 2)) 16)))
                  (4 (cffi:mem-ref pixels :uint32 point))
                  (otherwise (error "Unhandled bpp in surface-get-pixel")))))
    (cffi:with-foreign-objects ((r :uint8)
                                (g :uint8)
                                (b :uint8)
                                (a :uint8))
      (pal-ffi:get-rgba pixel (cffi:foreign-slot-value image 'pal-ffi:surface 'pal-ffi:pixelformat) r g b a)
      (values (cffi:mem-ref r :uint8)
              (cffi:mem-ref g :uint8)
              (cffi:mem-ref b :uint8)
              (cffi:mem-ref a :uint8)))))

(defun image-from-array (array smoothp)
  (image-from-fn (array-dimension array 0)
                 (array-dimension array 1)
                 smoothp
                 (lambda (x y)
                   (let ((pixel (aref array x y)))
                     (values (first pixel)
                             (second pixel)
                             (third pixel)
                             (fourth pixel))))))

(defun image-from-fn (width height smoothp fn)
  (close-quads)
  (let* ((mode pal-ffi:+gl-rgb+)
         (width (min *max-texture-size* width))
         (height (min *max-texture-size* height))
         (texture-width (expt 2 (ceiling (/ (log width)
                                            (log 2)))))
         (texture-height (expt 2 (ceiling (/ (log height)
                                             (log 2)))))
         (id (cffi:foreign-alloc :uint :count 1)))
    (with-foreign-vector (tdata (* texture-width texture-height) 4)
      (do-n (x width y height)
        (multiple-value-bind (r g b a) (funcall fn x y)
          (let ((a (or a 255))
                (p (the fixnum (+ (* y texture-width 4)
                                  (the u16 (* 4 x))))))
            (when (< a 255)
              (setf mode pal-ffi:+gl-rgba+))
            (setf (cffi:mem-ref tdata :uint8 p) (the u8 r)
                  (cffi:mem-ref tdata :uint8 (+ p 1)) (the u8 g)
                  (cffi:mem-ref tdata :uint8 (+ p 2)) (the u8 b)
                  (cffi:mem-ref tdata :uint8 (+ p 3)) (the u8 a)))))
      (pal-ffi:gl-gen-textures 1 id)
      (pal-ffi:gl-bind-texture pal-ffi:+gl-texture-2d+ (cffi:mem-ref id :uint))
      (pal-ffi:gl-tex-parameteri pal-ffi:+gl-texture-2d+ pal-ffi:+gl-texture-mag-filter+ (if smoothp pal-ffi:+gl-linear+ pal-ffi:+gl-nearest+))
      (pal-ffi:gl-tex-parameteri pal-ffi:+gl-texture-2d+ pal-ffi:+gl-texture-min-filter+ (if smoothp pal-ffi:+gl-linear+ pal-ffi:+gl-nearest+))
      (pal-ffi:gl-teximage2d pal-ffi:+gl-texture-2d+
                             0
                             mode
                             texture-width texture-height 0 pal-ffi:+gl-rgba+ pal-ffi:+gl-unsigned-byte+ tdata))
    (let ((image (pal-ffi::make-image :texture (cffi:mem-ref id :uint)
                                      :tx2 (coerce (/ width texture-width) 'single-float)
                                      :ty2 (coerce (/ height texture-height) 'single-float)
                                      :texture-width texture-width
                                      :texture-height texture-height
                                      :width width
                                      :height height)))
      (setf *current-image* image)
      (cffi:foreign-free id)
      (pal-ffi:register-resource image))))

(defun load-image-to-array (file)
  (let* ((surface (pal-ffi:load-image (data-path file))))
    (assert (not (cffi:null-pointer-p surface)))
    (let* ((width (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:w))
           (height (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:h))
           (array (make-array (list width height))))
      (do-n (x width y height)
        (setf (aref array x y) (multiple-value-list (surface-get-pixel surface x y))))
      (pal-ffi:free-surface surface)
      array)))

(defun load-image (file &optional (smoothp nil))
  (let* ((surface (pal-ffi:load-image (data-path file)))
         (image (progn (assert (not (cffi:null-pointer-p surface)))
                       (image-from-fn (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:w)
                                      (cffi:foreign-slot-value surface 'pal-ffi:surface 'pal-ffi:h)
                                      smoothp
                                      (lambda (x y)
                                        (surface-get-pixel surface x y))))))
    (pal-ffi::free-surface surface)
    image))

(defunct screen-to-array (pos width height)
    (vec pos u16 width u16 height)
  (close-quads)
  (pal-ffi:gl-flush)
  (let* ((x (truncate (vx pos)))
         (y (truncate (vy pos)))
         (rowsize (* width 4))
         (array (make-array (list width height))))
    (cffi:with-foreign-object (image :unsigned-char (* (1+ width) (1+ height) 4))
      (pal-ffi:gl-read-pixels x
                              (- *height* y height)
                              width height
                              pal-ffi:+gl-rgba+ pal-ffi:+gl-unsigned-byte+
                              image)
      (do-n (x width y height)
        (let ((yrow (* y rowsize)))
          (declare (type fixnum yrow))
          (setf (aref array x (- height y 1))
                (list (cffi:mem-aref image :unsigned-char (+ yrow
                                                             (* x 4)
                                                             0))
                      (cffi:mem-aref image :unsigned-char (+ yrow
                                                             (* x 4)
                                                             1))
                      (cffi:mem-aref image :unsigned-char (+ yrow
                                                             (* x 4)
                                                             2))
                      (cffi:mem-aref image :unsigned-char (+ yrow
                                                             (* x 4)
                                                             3))))))
      array)))


(defunct draw-image (image pos &key angle scale valign halign vmirror hmirror)
    (image image vec pos (or boolean number) angle (or boolean number) scale symbol halign symbol valign symbol vmirror symbol hmirror)
  (set-image image)
  (let ((width (image-width image))
        (height (image-height image))
        (tx1 (if hmirror (pal-ffi:image-tx2 image) 0f0))
        (ty1 (if vmirror (pal-ffi:image-ty2 image) 0f0))
        (tx2 (if hmirror 0f0 (pal-ffi:image-tx2 image)))
        (ty2 (if vmirror 0f0 (pal-ffi:image-ty2 image))))
    (if (or angle scale valign halign)
        (with-transformation ()
          (translate pos)
          (when angle
            (rotate angle))
          (when scale
            (scale scale scale)) ;; :-)
          (let ((x (case halign
                     (:right (coerce (- width) 'single-float))
                     (:left 0f0)
                     (:middle (- (/ width 2f0)))
                     (otherwise 0f0)))
                (y (case valign
                     (:bottom (coerce (- height) 'single-float))
                     (:top 0f0)
                     (:middle (- (/ height 2f0)))
                     (otherwise 0f0))))
            (with-gl pal-ffi:+gl-quads+
              (pal-ffi:gl-tex-coord2f tx1 ty1)
              (pal-ffi:gl-vertex2f x y)
              (pal-ffi:gl-tex-coord2f tx2 ty1)
              (pal-ffi:gl-vertex2f (+ x width) y)
              (pal-ffi:gl-tex-coord2f tx2 ty2)
              (pal-ffi:gl-vertex2f (+ x width) (+ y height))
              (pal-ffi:gl-tex-coord2f tx1 ty2)
              (pal-ffi:gl-vertex2f x (+ y height)))))
        (let* ((x (vx pos))
               (y (vy pos))
               (width (+ x width))
               (height (+ y height)))
          (with-gl pal-ffi:+gl-quads+
            (pal-ffi:gl-tex-coord2f tx1 ty1)
            (pal-ffi:gl-vertex2f x y)
            (pal-ffi:gl-tex-coord2f tx2 ty1)
            (pal-ffi:gl-vertex2f width y)
            (pal-ffi:gl-tex-coord2f tx2 ty2)
            (pal-ffi:gl-vertex2f width height)
            (pal-ffi:gl-tex-coord2f tx1 ty2)
            (pal-ffi:gl-vertex2f x height))))))


(defunct draw-image* (image from-pos to-pos width height)
    (image image vec from-pos vec to-pos u11 width u11 height)
  (set-image image)
  (let* ((vx (vx from-pos))
         (vy (vy from-pos))
         (x1 (vx to-pos))
         (y1 (vy to-pos))
         (x2 (+ x1 width))
         (y2 (+ y1 height))
         (tx1 (/ vx (pal-ffi:image-texture-width image)))
         (ty1 (/ vy (pal-ffi:image-texture-height image)))
         (tx2 (/ (+ vx width) (pal-ffi:image-texture-width image)))
         (ty2 (/ (+ vy height) (pal-ffi:image-texture-height image))))
    (with-gl pal-ffi:+gl-quads+
      (pal-ffi:gl-tex-coord2f tx1 ty1)
      (pal-ffi:gl-vertex2f x1 y1)
      (pal-ffi:gl-tex-coord2f tx2 ty1)
      (pal-ffi:gl-vertex2f x2 y1)
      (pal-ffi:gl-tex-coord2f tx2 ty2)
      (pal-ffi:gl-vertex2f x2 y2)
      (pal-ffi:gl-tex-coord2f tx1 ty2)
      (pal-ffi:gl-vertex2f x1 y2))))

(declaim (inline draw-line))
(defunct draw-line (la lb r g b a &key (size 1.0f0) (smoothp))
    (vec la vec lb single-float size u8 r u8 g u8 b u8 a boolean smoothp)
  (with-line-settings smoothp size r g b a
    (with-gl pal-ffi:+gl-lines+
      (pal-ffi:gl-vertex2f (vx la) (vy la))
      (pal-ffi:gl-vertex2f (vx lb) (vy lb)))))


(declaim (inline draw-arrow))
(defunct draw-arrow (la lb r g b a &key (size 1.0f0) smoothp)
    (vec la vec lb u8 r u8 g u8 b u8 a single-float size boolean smoothp)
  (with-line-settings smoothp size r g b a
    (let ((d (v* (v-direction la lb) (+ size 8f0))))
      (with-gl pal-ffi:+gl-lines+
        (pal-ffi:gl-vertex2f (vx la) (vy la))
        (pal-ffi:gl-vertex2f (vx lb) (vy lb))
        (pal-ffi:gl-vertex2f (vx lb) (vy lb))
        (pal-ffi:gl-vertex2f (vx (v+ lb (v-rotate d 140f0)))
                             (vy (v+ lb (v-rotate d 140f0))))
        (pal-ffi:gl-vertex2f (vx lb) (vy lb))
        (pal-ffi:gl-vertex2f (vx (v+ lb (v-rotate d -140f0)))
                             (vy (v+ lb (v-rotate d -140f0))))))))



(declaim (inline draw-point))
(defunct draw-point (pos r g b a &key (size 1f0) smoothp)
    (vec pos u8 r u8 g u8 b u8 a single-float size boolean smoothp)
  (close-quads)
  (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-current-bit+ pal-ffi:+gl-enable-bit+))
  (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
  (if smoothp
      (pal-ffi:gl-enable pal-ffi:+gl-point-smooth+)
      (pal-ffi:gl-disable pal-ffi:+gl-point-smooth+))
  (pal-ffi:gl-point-size size)
  (pal-ffi:gl-color4ub r g b a)
  (with-gl pal-ffi:+gl-point+
    (pal-ffi:gl-vertex2f (vx pos) (vy pos)))
  (pal-ffi:gl-pop-attrib))

(defunct draw-rectangle (pos width height r g b a &key (fill t) (size 1f0) absolutep smoothp)
    (vec pos u16 width u16 height u8 r u8 g u8 b u8 a (or symbol image) fill single-float size boolean absolutep boolean smoothp)
  (cond
    ((image-p fill)
     (draw-polygon (list pos
                         (v+ pos (v width 0))
                         (v+ pos (v width height))
                         (v+ pos (v 0 height)))
                   r g b a
                   :fill fill
                   :smoothp smoothp
                   :absolutep absolutep))
    ((eq nil fill)
     (with-line-settings smoothp size r g b a
       (with-gl pal-ffi:+gl-line-loop+
         (pal-ffi:gl-vertex2f (vx pos) (vy pos))
         (pal-ffi:gl-vertex2f (+ (vx pos) width) (vy pos))
         (pal-ffi:gl-vertex2f (+ (vx pos) width) (+ (vy pos) height))
         (pal-ffi:gl-vertex2f (vx pos) (+ (vy pos) height)))))
    (t
     (close-quads)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-current-bit+ pal-ffi:+gl-enable-bit+))
     (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
     (pal-ffi:gl-color4ub r g b a)
     (pal-ffi:gl-rectf (vx pos) (vy pos) (+ (vx pos) width) (+ (vy pos) height))
     (pal-ffi:gl-pop-attrib))))

(defunct draw-polygon (points r g b a &key (fill t) absolutep (size 1f0) smoothp)
    (list points u8 r u8 g u8 b u8 a (or image boolean) fill single-float size)
  (cond
    ((image-p fill)
     (close-quads)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-current-bit+))
     (when smoothp
       (pal-ffi:gl-enable pal-ffi:+gl-polygon-smooth+))
     (set-image fill)
     (pal-ffi:gl-color4ub r g b a)
     (with-gl pal-ffi:+gl-polygon+
       (let ((dx (vx (first points)))
             (dy (vy (first points))))
         (dolist (p points)
           (let* ((x (vx p))
                  (y (vy p))
                  (tx (/ (if absolutep
                             x
                             (- x dx))
                         (pal-ffi:image-texture-width fill)))
                  (ty (/ (if absolutep
                             y
                             (- y dy))
                         (pal-ffi:image-texture-height fill))))
             (pal-ffi:gl-tex-coord2f tx ty)
             (pal-ffi:gl-vertex2f x y)))))
     (pal-ffi:gl-pop-attrib))
    ((eq nil fill)
     (with-line-settings smoothp size r g b a
       (with-gl pal-ffi:+gl-line-loop+
         (dolist (p points)
           (pal-ffi:gl-vertex2f (vx p) (vy p))))))
    (t
     (close-quads)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-current-bit+ pal-ffi:+gl-enable-bit+))
     (pal-ffi:gl-color4ub r g b a)
     (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
     (when smoothp
       (pal-ffi:gl-enable pal-ffi:+gl-polygon-smooth+))
     (with-gl pal-ffi:+gl-polygon+
       (dolist (p points)
         (pal-ffi:gl-vertex2f (vx p) (vy p))))
     (pal-ffi:gl-pop-attrib))))

(defunct draw-polygon* (points &key image tex-coords colors smoothp)
    (list points list tex-coords list colors (or boolean image) image boolean smoothp)
  (close-quads)
  (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-current-bit+ pal-ffi:+gl-enable-bit+))
  (cond
    ((and image tex-coords)
     (set-image image)
     (when smoothp
       (pal-ffi:gl-enable pal-ffi:+gl-polygon-smooth+))
     (cond
       (colors
        (pal-ffi:gl-shade-model pal-ffi:+gl-smooth+)
        (with-gl pal-ffi:+gl-polygon+
          (loop
             for p in points
             for tc in tex-coords
             for c in colors
             do
             (pal-ffi:gl-tex-coord2f (/ (vx tc) (pal-ffi:image-texture-width image)) (/ (vy tc) (pal-ffi:image-texture-height image)))
             (pal-ffi:gl-color4ub (first c) (second c) (third c) (fourth c))
             (pal-ffi:gl-vertex2f (vx p) (vy p)))))
       (t
        (with-gl pal-ffi:+gl-polygon+
          (loop
             for p in points
             for tc in tex-coords
             do
             (pal-ffi:gl-tex-coord2f (/ (vx tc) (pal-ffi:image-texture-width image)) (/ (vy tc) (pal-ffi:image-texture-height image)))
             (pal-ffi:gl-vertex2f (vx p) (vy p)))))))
    (t
     (pal-ffi:gl-shade-model pal-ffi:+gl-smooth+)
     (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
     (when smoothp
       (pal-ffi:gl-enable pal-ffi:+gl-polygon-smooth+))
     (with-gl pal-ffi:+gl-polygon+
       (loop
          for p in points
          for c in colors
          do
          (pal-ffi:gl-color4ub (first c) (second c) (third c) (fourth c))
          (pal-ffi:gl-vertex2f (vx p) (vy p))))))
  (pal-ffi:gl-pop-attrib))

(defunct draw-circle (pos radius r g b a &key (fill t) absolutep (size 1f0) smoothp (segments 30))
    (vec pos single-float radius u8 r u8 g u8 b u8 a
         (or image symbol) fill boolean absolutep single-float size boolean smoothp fixnum segments)
  (declare (type vec pos) (type fixnum segments))
  (draw-polygon (loop for a from 0 to (* 2 pi) by (/ (* 2 pi) segments) collecting
                     (v+ pos
                         (v (* (sin a) radius)
                            (* (cos a) radius))))
                r g b a :fill fill :absolutep absolutep :size size :smoothp smoothp))

;;; Samples

(defun load-sample (file &optional (volume 255))
  "Volume 0-255"
  (let ((sample (pal-ffi:load-wav (data-path file))))
    (assert (not (cffi:null-pointer-p sample)))
    (let ((sample (pal-ffi:register-resource (pal-ffi::make-sample :chunk sample))))
      (pal-ffi:volume-chunk (pal-ffi:sample-chunk sample) (1+ (truncate volume 2)))
      sample)))

(declaim (inline play-sample))
(defun play-sample (sample &key (loops nil) (angle 0) (volume 255))
  "Loops is: t = forever, nil = once, number = number of loops. Angle is an integer between 0-360. Volume is an integer between 0-255."
  (let ((channel (pal-ffi:play-channel -1 (pal-ffi:sample-chunk sample) (cond
                                                                          ((numberp loops)
                                                                           loops)
                                                                          ((eq t loops)
                                                                           -1)
                                                                          (t 0)))))
    (pal-ffi:set-position channel (truncate angle) (- 255 volume))
    channel))

(defun set-sample-volume (sample volume)
  "Volume 0-255"
  (pal-ffi:volume-chunk (pal-ffi:sample-chunk sample) (1+ (truncate volume 2))))



;;; Music

(defun load-music (file)
  (let ((music (pal-ffi:load-music (data-path file))))
    (assert (not (cffi:null-pointer-p music)))
    (let ((music (pal-ffi::make-music :music music)))
      (pal-ffi:register-resource music))))

(defun play-music (music &key (loops t) (volume 255) (fade 0))
  "Volume 0-255. Loops is: t = forever, nil = once, number = number of loops"
  (pal-ffi:volume-music (1+ (truncate volume 2)))
  (if (> fade 0)
      (pal-ffi:fade-in-music (pal-ffi:music-music music) (cond ((eq loops t) -1)
                                                               ((null loops) 0)
                                                               (t (truncate loops)))
                             fade)
      (pal-ffi:play-music (pal-ffi:music-music music) (cond ((eq loops t) -1)
                                                            ((null loops) 0)
                                                            (t (truncate loops))))))

(defun set-music-volume (volume)
  "Volume 0-255"
  (pal-ffi:volume-music (1+ (truncate volume 2))))

(defun halt-music (&optional fade)
  (if fade
      (pal-ffi:fade-out-music fade)
      (pal-ffi:halt-music)))




;; Fonts

(defstruct glyph
  (char #\space :type character)
  (pos (v 0 0) :type vec)
  (width 0 :type u11)
  (height 0 :type u11)
  (xoff 0 :type fixnum))


(defun load-font (font)
  (let ((glyphs (make-array 256 :initial-element (make-glyph :width 1 :height 1 :xoff 0) :element-type 'glyph))
        (lines (with-open-file (file (data-path (concatenate 'string font ".fnt")))
                 (loop repeat 4 do (read-line file))
                 (loop for i from 0 to 94 collecting
                      (substitute #\space #\, (subseq (read-line file) 6) :start 1)))))
    (dolist (line lines)
      (let ((glyph (glyph-from-line line)))
        (setf (aref glyphs (char-code (glyph-char glyph)))
              glyph)))
    (let ((font (pal-ffi:register-resource (pal-ffi:make-font :image (load-image (concatenate 'string font ".png"))
                                                              :height (glyph-height (aref glyphs 32))
                                                              :glyphs glyphs))))
      (set-image (pal-ffi:font-image font))
      font)))

(defun glyph-from-line (line)
  (let ((char (elt line 0))
        (coords (read-from-string (concatenate 'string "(" (subseq line 2) ")"))))
    (make-glyph :char char
                :pos (v (first coords)
                        (second coords))
                :width (third coords)
                :height (fourth coords)
                :xoff (sixth coords))))

(defun draw-glyph (x height image g)
  (declare (type single-float x height) (type image image) (type glyph g))
  (let* ((vx (vx (glyph-pos g)))
         (vy (vy (glyph-pos g)))
         (width (coerce (glyph-width g) 'single-float))
         (tx1 (/ vx (pal-ffi:image-texture-width image)))
         (ty1 (/ vy (pal-ffi:image-texture-height image)))
         (tx2 (/ (+ vx width) (pal-ffi:image-texture-width image)))
         (ty2 (/ (+ vy height) (pal-ffi:image-texture-height image))))
    (pal-ffi:gl-tex-coord2f tx1 ty1)
    (pal-ffi:gl-vertex2f x 0f0)
    (pal-ffi:gl-tex-coord2f tx2 ty1)
    (pal-ffi:gl-vertex2f (+ x width) 0f0)
    (pal-ffi:gl-tex-coord2f tx2 ty2)
    (pal-ffi:gl-vertex2f (+ x width) height)
    (pal-ffi:gl-tex-coord2f tx1 ty2)
    (pal-ffi:gl-vertex2f x height)
    (+ (glyph-width g) (glyph-xoff g))))

(defunct draw-text (text pos &optional font)
    (vec pos simple-string text (or font boolean) font)
  (with-transformation (:pos pos)
    (let* ((dx 0f0)
           (font (if font
                     font
                     (tag 'default-font)))
           (height (coerce (pal-ffi:font-height font) 'single-float)))
      (set-image (pal-ffi:font-image font))
      (with-gl pal-ffi:+gl-quads+
        (loop for char across text do
             (incf dx
                   (draw-glyph dx height (pal-ffi:font-image font) (aref (pal-ffi:font-glyphs font) (char-code char)))))))))

(declaim (inline get-font-height))
(defunct get-font-height (&optional font)
    ((or font boolean) font)
  (pal-ffi:font-height (or font (tag 'default-font))))

(defunct get-text-size (text &optional font)
    ((or font boolean) font simple-string text)
  (values (let ((glyphs (pal-ffi:font-glyphs (if font
                                                 font
                                                 (tag 'default-font)))))
            (loop for c across text summing
                 (+ (glyph-width (aref glyphs (char-code c)))
                    (glyph-xoff (aref glyphs (char-code c))))))
          (pal-ffi:font-height (if font
                                   font
                                   (tag 'default-font)))))

(defun draw-fps (&optional font)
  (draw-text (prin1-to-string (get-fps)) (v 0 0) font))

(defun message (&rest messages)
  (setf *messages* (append *messages* (list (format nil "~{~S ~}" messages))))
  (when (> (length *messages*) (- (truncate (get-screen-height) (get-font-height)) 1))
    (pop *messages*)))