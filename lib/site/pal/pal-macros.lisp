(declaim (optimize (speed 3)
                   (safety 1)))

(in-package :pal)


;; TAGs are lazily evaluated thunks that load some resource (image, font etc.) when called with (TAG tag-name).
;; Their values are cached and automatically cleaned when resource is freed.
;; NOTE: Once defined the TAG definitions persist thru the whole Lisp session. Only the result values get initialized.


(defvar *tags* (make-hash-table :test 'eq)
  "*TAGS* is a hashtable of TAG-NAME -> (FUNCTION . RESOURCE) we use to hold TAGS.")

(defmacro define-tags (&body tags)
  `(progn
     ,@(mapcar (lambda (r)
                 `(add-tag ',(first r) (lambda () ,(second r))))
               (loop for (a b) on tags by #'cddr collect (list a b)))))


(defun add-tag (tag fn)
  (assert (and (symbolp tag) (functionp fn)))
  (setf (gethash tag *tags*)
        (cons fn nil)))


;; Clean all the values from tag table. Internal use only!
(defun reset-tags (&key resource)
  (maphash (if resource
               (lambda (k v)
                 (declare (ignore k))
                 (when (eq resource (cdr v))
                   (setf (cdr v) nil)))
               (lambda (k v)
                 (declare (ignore k))
                 (setf (cdr v) nil)))
           *tags*))



(defun tag (name)
  (declare (type symbol name))
  (let ((resource (gethash name *tags*)))
    (if resource
        (if (cdr resource)
            (the resource (cdr resource))
            (let ((r (funcall (car resource))))
              (assert (resource-p r))
              (the resource (setf (cdr resource) r))))
        (error "Named resource ~a not found" name))))

(defun make-coerce-form (to-type value)
  `(,value ,(case to-type
                  ((u8 u11 u16 integer fixnum) `(truncate ,value))
                  (component `(coerce ,value 'component))
                  (single-float `(coerce ,value 'single-float))
                  (double-float `(coerce ,value 'double-float))
                  (float `(coerce ,value 'float)))))


;; Messy. Like DEFUN but automatically coerce some types (defined up there -^ ) and declare their types.
(defmacro defunct (name lambda-list declarations &body body)
  (let* ((decls (loop for (a b) on declarations by #'cddr collecting
                     `(type ,a ,b)))
         (coerced (remove-if (lambda (decl)
                               (null (second decl)))
                             (mapcar (lambda (decl)
                                       (make-coerce-form (second decl) (third decl)))
                                     decls))))
    (if coerced
        `(defun ,name ,lambda-list
           (let (,@coerced)
             (declare ,@decls)
             ,@body))
        `(defun ,name ,lambda-list
           (declare ,@decls)
           ,@body))))



(defmacro with-resource ((resource init-form) &body body)
  "Bind the result of INIT-FORM to RESOURCE, evaluate the BODY and free the RESOURCE."
  `(let ((,resource ,init-form))
     (prog1 (progn
              ,@body)
       (pal:free-resource ,resource))))


(defmacro with-default-settings (&body body)
  "Evaluate BODY with default transformations and blend settings."
  `(with-transformation ()
     (with-blend (:mode :blend :color +white+)
       (pal-ffi:gl-load-identity)
       ,@body)))


(defmacro with-blend ((&key (mode t) color) &body body)
  "Evaluate BODY with blend options set to MODE and COLOR."
  `(progn
     (close-quads)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-color-buffer-bit+ pal-ffi:+gl-current-bit+ pal-ffi:+gl-enable-bit+))
     ,(unless (eq mode t)
              `(set-blend-mode ,mode))
     ,(when color
            `(set-blend-color ,color))
     (prog1 (progn
              ,@body)
       (close-quads)
       (pal-ffi:gl-pop-attrib))))

(defmacro with-clipping ((x y width height) &body body)
  "Evaluate BODY with clipping. Only the window area defined by X, Y, WIDTH and HEIGHT is affected by drawing primitives."
  `(progn
     (push-clip ,x ,y ,width ,height)
     (prog1 (progn
              ,@body)
       (pop-clip))))

(defmacro with-transformation ((&key pos angle scale) &body body)
  "Evaluate BODY with translation POS, rotation ANGLE and scaling SCALE. Transformations are applied in that order."
  `(progn
     (close-quads)
     (pal-ffi:gl-push-matrix)
     ,(when pos
            `(translate ,pos))
     ,(when angle
            `(rotate ,angle))
     ,(when scale
            (let ((s (gensym)))
              `(let ((,s ,scale))
                 (scale ,s ,s))))
     (prog1 (progn
              ,@body)
       (close-quads)
       (pal-ffi:gl-pop-matrix))))

(defmacro with-gl (mode &body body)
  "Wrap BODY between (gl-begin MODE) and (gl-end). When used with +GL-QUADS+ gl-begin/end are possibly completely left out."
  (if (eq mode 'pal-ffi:+gl-quads+)
      `(progn
         (open-quads)
         ,@body)
      `(progn
         (close-quads)
         (pal-ffi:gl-begin ,mode)
         ,@body
         (pal-ffi:gl-end))))

(defmacro with-line-settings (smoothp size r g b a &body body)
  `(progn
     (close-quads)
     (pal-ffi:gl-push-attrib (logior pal-ffi:+gl-current-bit+ pal-ffi:+gl-line-bit+ pal-ffi:+gl-enable-bit+))
     (pal-ffi:gl-disable pal-ffi:+gl-texture-2d+)
     (set-blend-color (color ,r ,g ,b ,a))
     (pal-ffi:gl-line-width ,size)
     (if ,smoothp
         (pal-ffi:gl-enable pal-ffi:+gl-line-smooth+)
         (pal-ffi:gl-disable pal-ffi:+gl-line-smooth+))
     ,@body
     (pal-ffi:gl-pop-attrib)))

(defmacro randomly (p &body body)
  "There is a 1/P chance of the BODY to be evaluated."
  `(when (= (random ,p) 0)
     ,@body))

(defmacro do-n ((&rest args) &body body)
  (labels ((expand (args)
             (cond
               ((null args) `(progn ,@body))
               (t `(dotimes ,(list (first args) (second args))
                     (declare (type fixnum ,(first args)))
                     ,(expand (cddr args)))))))
    (expand args)))


(defmacro test-keys (&body args)
  `(progn
     ,@(mapcar (lambda (arg)
                 `(when ,(if (listp (first arg))
                             `(or ,@(mapcar (lambda (a)
                                              (list 'key-pressed-p a))
                                            (first arg)))
                             `(key-pressed-p ,(first arg)))
                    ,@(rest arg)))
               args)))

(declaim (inline funcall?))
(defun funcall? (fn &rest args)
  (declare (type (or function symbol) fn) (dynamic-extent args))
  (if (null fn)
      nil
      (apply fn args)))

;; Messy...
(defmacro do-event (event key-up-fn key-down-fn mouse-btn-up-fn mouse-btn-down-fn mouse-motion-fn quit-fn)
  `(loop while (pal-ffi:poll-event ,event)
      do
      (let ((type (cffi:mem-ref ,event :uint8)))
        (cond

          ((= type pal-ffi:+key-up-event+)
           (let* ((keysym (cffi:foreign-slot-value ,event 'pal-ffi:keyboard-event 'pal-ffi:keysym))
                  (sym (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym))))
             (setf (gethash sym *pressed-keys*)
                   nil)
             (funcall? ,key-up-fn sym)))

          ((= type pal-ffi:+key-down-event+)
           (let* ((keysym (cffi:foreign-slot-value ,event 'pal-ffi:keyboard-event 'pal-ffi:keysym))
                  (sym (cffi:foreign-enum-keyword 'pal-ffi:sdl-key (cffi:foreign-slot-value keysym 'pal-ffi:keysym 'pal-ffi:sym))))
             (setf (gethash sym *pressed-keys*)
                   t)
             (if ,key-down-fn
                 (funcall ,key-down-fn sym)
                 (when (eq sym :key-escape)
                   (return-from event-loop)))))

          ((= type pal-ffi:+mouse-motion-event+)
           (setf *mouse-x* (cffi:foreign-slot-value ,event 'pal-ffi:mouse-motion-event 'pal-ffi:x)
                 *mouse-y* (cffi:foreign-slot-value ,event 'pal-ffi:mouse-motion-event 'pal-ffi:y))
           (funcall? ,mouse-motion-fn *mouse-x* *mouse-y*))

          ((= type pal-ffi:+mouse-button-up-event+)
           (let* ((button (cffi:foreign-slot-value ,event 'pal-ffi:mouse-button-event 'pal-ffi:button))
                  (keysym (read-from-string (format nil ":key-mouse-~a" button)))) ;; Mousekeys are handled as keycodes :KEY-MOUSE-n
             (setf (gethash keysym *pressed-keys*)
                   nil)
             (funcall? ,mouse-btn-up-fn button (v *mouse-x* *mouse-y*))))

          ((= type pal-ffi:+mouse-button-down-event+)
           (let* ((button (cffi:foreign-slot-value ,event 'pal-ffi:mouse-button-event 'pal-ffi:button))
                  (keysym (read-from-string (format nil ":key-mouse-~a" button))))
             (setf (gethash keysym *pressed-keys*)
                   t)
             (funcall? ,mouse-btn-down-fn button (v *mouse-x* *mouse-y*))))

          ((= type pal-ffi:+quit-event+)
           (if ,quit-fn
               (funcall ,quit-fn)
               (return-from event-loop))
           )))))


(defmacro event-loop ((&key key-up-fn key-down-fn mouse-btn-up-fn mouse-btn-down-fn mouse-motion-fn quit-fn) &body redraw)
  (let ((event (gensym)))
    `(block event-loop
       (cffi:with-foreign-object (,event :char 500)
         (loop
            (do-event ,event ,key-up-fn ,key-down-fn ,mouse-btn-up-fn ,mouse-btn-down-fn ,mouse-motion-fn ,quit-fn)
            ,@redraw
            (update-screen))))))


(defmacro with-pal (args &body body)
  "Open PAL and evaluate BODY. After BODY returns call CLOSE-PAL."
  `(progn
     (apply 'open-pal (list ,@args))
     (unwind-protect
          (progn ,@body)
       (close-pal))))


(defmacro with-foreign-vector ((chunk n size) &body body)
  `(let ((,chunk (pal-ffi:calloc ,n ,size)))
     (unwind-protect
          ,@body
       (pal-ffi:free ,chunk))))
