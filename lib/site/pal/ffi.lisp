(declaim (optimize (speed 3)
                   (safety 1)))

(in-package :pal-ffi)


(cffi:define-foreign-library sdl
  (:windows "SDL")
  (:unix (:or "libSDL-1.2.so.0" "libSDL-1.2.so")))

(cffi:define-foreign-library sdl-mixer
  (:windows "SDL_mixer")
  (:unix (:or "libSDL_mixer-1.2.so.0" "libSDL_mixer-1.2.so")))

(cffi:define-foreign-library sdl-image
  (:windows "SDL_image")
  (:unix (:or "libSDL_image-1.2.so.0" "libSDL_image-1.2.so")))

(cffi:define-foreign-library opengl
  (:windows "opengl32.dll")
  (:unix (:or "libGL.so")))

#+win32 (cffi:define-foreign-library shell32
          (:windows "shell32.dll")) ;; We use a function from shell32.dll to find the users application data directory.

(defun load-foreign-libraries ()
  "Load all the foreing libs. Useful when dumping and restarting images with CLisp."
  (cffi:use-foreign-library sdl)
  (cffi:use-foreign-library sdl-mixer)
  (cffi:use-foreign-library sdl-image)
  (cffi:use-foreign-library opengl)
  #+win32 (cffi:use-foreign-library shell32))

(load-foreign-libraries)


(deftype u8 () '(unsigned-byte 8))
(deftype u11 () '(unsigned-byte 11))
(deftype u16 () '(unsigned-byte 16))



;; Basic SDL ffi definitions

(defconstant +init-audio+ #x00000010)
(defconstant +init-video+ #x00000020)
(defconstant +fullscreen+ #x80000000)
(defconstant +opengl+ #x00000002)

(defconstant +audio-s8+ #x8008)
(defconstant +audio-s16lsb+ #x8010)
(defconstant +audio-s16msb+ #x9010)
(defconstant +audio-s16+ +audio-s16lsb+)
(defconstant +channels+ 8)
(defconstant +default-frequency+ 22050)
(defconstant +default-channels+ 2)
(defconstant +max-value+ 128)
(defconstant +channel-post+ -2)
(defconstant +no-fading+ 0)
(defconstant +fading-out+ 1)
(defconstant +fading-in+ 2)
(defconstant +mus-none+ 0)
(defconstant +mus-cmd+ 1)
(defconstant +mus-wav+ 2)
(defconstant +mus-mod+ 3)
(defconstant +mus-mid+ 4)
(defconstant +mus-ogg+ 5)
(defconstant +mus-mp3+ 6)

(defconstant +button-left+ 1)
(defconstant +button-middle+ 2)
(defconstant +button-right+ 3)
(defconstant +button-wheelup+ 4)
(defconstant +button-wheeldown+ 5)



(cffi:defcstruct rectangle
  (x :short)
  (y :short)
  (w :uint16)
  (h :uint16))

(cffi:defcstruct color
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (unused :uint8))

(cffi:defcstruct surface
  (flags :uint)
  (pixelformat :pointer)
  (w :int)
  (h :int)
  (pitch :uint16)
  (pixels :pointer)
  (offset :int)
  (hwdata :pointer)
  (clip-rect rectangle)
  (unused1 :uint)
  (locked :uint)
  (map :pointer)
  (format-version :uint)
  (refcount :int))

(cffi:defcstruct pixelformat
  (palette :pointer)
  (BitsPerPixel :uint8)
  (BytesPerPixel :uint8)
  (Rloss :uint8)
  (Gloss :uint8)
  (Bloss :uint8)
  (Aloss :uint8)
  (Rshift :uint8)
  (Gshift :uint8)
  (Bshift :uint8)
  (Ashift :uint8)
  (Rmask :uint)
  (Gmask :uint)
  (Bmask :uint)
  (Amask :uint)
  (colorkey :uint)
  (alpha :uint8))

(cffi:defcstruct keysym
  (scancode :uint8)
  (sym :int)
  (mod :int)
  (unicode :uint16))

(cffi:defcstruct keyboard-event
  (type :uint8)
  (state :uint8)
  (keysym keysym))

(cffi:defcstruct mouse-button-event
  (type :uint8)
  (which :uint8)
  (button :uint8)
  (state :uint8)
  (x :uint16) (y :uint16))

(cffi:defcstruct mouse-motion-event
  (type :uint8)
  (which :uint8)
  (state :uint8)
  (x :uint16) (y :uint16)
  (xrel :int16) (yrel :int16))

(cffi:defcstruct quit-event
  (type :uint8))

(cffi:defcstruct active-event
  (type :uint8)
  (gain :uint8)
  (state :uint8))

(cffi:defcstruct resize-event
  (type :uint8)
  (w :int) (h :int))



(defconstant +no-event+ 0)
(defconstant +active-event+ 1)
(defconstant +key-down-event+ 2)
(defconstant +key-up-event+ 3)
(defconstant +mouse-motion-event+ 4)
(defconstant +mouse-button-down-event+ 5)
(defconstant +mouse-button-up-event+ 6)
(defconstant +quit-event+ 12)
(defconstant +resize-event+ 16)
(defconstant +expose-event+ 17)


;; Keycodes used by PAL.
;; In addition to these :KEY-MOUSE-n are used for mousekeys.
(cffi:defcenum sdl-key
  (:key-unknown 0)
  (:key-first 0)
  (:key-backspace 8)
  (:key-tab 9)
  (:key-clear 12)
  (:key-return 13)
  (:key-pause 19)
  (:key-escape 27)
  (:key-space 32)
  (:key-exclaim 33)
  (:key-quotedbl 34)
  (:key-hash 35)
  (:key-dollar 36)
  (:key-ampersand 38)
  (:key-quote 39)
  (:key-leftparen 40)
  (:key-rightparen 41)
  (:key-asterisk 42)
  (:key-plus 43)
  (:key-comma 44)
  (:key-minus 45)
  (:key-period 46)
  (:key-slash 47)
  (:key-0 48)
  (:key-1 49)
  (:key-2 50)
  (:key-3 51)
  (:key-4 52)
  (:key-5 53)
  (:key-6 54)
  (:key-7 55)
  (:key-8 56)
  (:key-9 57)
  (:key-colon 58)
  (:key-semicolon 59)
  (:key-less 60)
  (:key-equals 61)
  (:key-greater 62)
  (:key-question 63)
  (:key-at 64)
  (:key-leftbracket 91)
  (:key-backslash 92)
  (:key-rightbracket 93)
  (:key-caret 94)
  (:key-underscore 95)
  (:key-backquote 96)
  (:key-a 97)
  (:key-b 98)
  (:key-c 99)
  (:key-d 100)
  (:key-e 101)
  (:key-f 102)
  (:key-g 103)
  (:key-h 104)
  (:key-i 105)
  (:key-j 106)
  (:key-k 107)
  (:key-l 108)
  (:key-m 109)
  (:key-n 110)
  (:key-o 111)
  (:key-p 112)
  (:key-q 113)
  (:key-r 114)
  (:key-s 115)
  (:key-t 116)
  (:key-u 117)
  (:key-v 118)
  (:key-w 119)
  (:key-x 120)
  (:key-y 121)
  (:key-z 122)
  (:key-delete 127)
  (:key-world_0 160)
  (:key-world_1 161)
  (:key-world_2 162)
  (:key-world_3 163)
  (:key-world_4 164)
  (:key-world_5 165)
  (:key-world_6 166)
  (:key-world_7 167)
  (:key-world_8 168)
  (:key-world_9 169)
  (:key-world_10 170)
  (:key-world_11 171)
  (:key-world_12 172)
  (:key-world_13 173)
  (:key-world_14 174)
  (:key-world_15 175)
  (:key-world_16 176)
  (:key-world_17 177)
  (:key-world_18 178)
  (:key-world_19 179)
  (:key-world_20 180)
  (:key-world_21 181)
  (:key-world_22 182)
  (:key-world_23 183)
  (:key-world_24 184)
  (:key-world_25 185)
  (:key-world_26 186)
  (:key-world_27 187)
  (:key-world_28 188)
  (:key-world_29 189)
  (:key-world_30 190)
  (:key-world_31 191)
  (:key-world_32 192)
  (:key-world_33 193)
  (:key-world_34 194)
  (:key-world_35 195)
  (:key-world_36 196)
  (:key-world_37 197)
  (:key-world_38 198)
  (:key-world_39 199)
  (:key-world_40 200)
  (:key-world_41 201)
  (:key-world_42 202)
  (:key-world_43 203)
  (:key-world_44 204)
  (:key-world_45 205)
  (:key-world_46 206)
  (:key-world_47 207)
  (:key-world_48 208)
  (:key-world_49 209)
  (:key-world_50 210)
  (:key-world_51 211)
  (:key-world_52 212)
  (:key-world_53 213)
  (:key-world_54 214)
  (:key-world_55 215)
  (:key-world_56 216)
  (:key-world_57 217)
  (:key-world_58 218)
  (:key-world_59 219)
  (:key-world_60 220)
  (:key-world_61 221)
  (:key-world_62 222)
  (:key-world_63 223)
  (:key-world_64 224)
  (:key-world_65 225)
  (:key-world_66 226)
  (:key-world_67 227)
  (:key-world_68 228)
  (:key-world_69 229)
  (:key-world_70 230)
  (:key-world_71 231)
  (:key-world_72 232)
  (:key-world_73 233)
  (:key-world_74 234)
  (:key-world_75 235)
  (:key-world_76 236)
  (:key-world_77 237)
  (:key-world_78 238)
  (:key-world_79 239)
  (:key-world_80 240)
  (:key-world_81 241)
  (:key-world_82 242)
  (:key-world_83 243)
  (:key-world_84 244)
  (:key-world_85 245)
  (:key-world_86 246)
  (:key-world_87 247)
  (:key-world_88 248)
  (:key-world_89 249)
  (:key-world_90 250)
  (:key-world_91 251)
  (:key-world_92 252)
  (:key-world_93 253)
  (:key-world_94 254)
  (:key-world_95 255)
  (:key-kp0 256)
  (:key-kp1 257)
  (:key-kp2 258)
  (:key-kp3 259)
  (:key-kp4 260)
  (:key-kp5 261)
  (:key-kp6 262)
  (:key-kp7 263)
  (:key-kp8 264)
  (:key-kp9 265)
  (:key-kp_period 266)
  (:key-kp_divide 267)
  (:key-kp_multiply 268)
  (:key-kp_minus 269)
  (:key-kp_plus 270)
  (:key-kp_enter 271)
  (:key-kp_equals 272)
  (:key-up 273)
  (:key-down 274)
  (:key-right 275)
  (:key-left 276)
  (:key-insert 277)
  (:key-home 278)
  (:key-end 279)
  (:key-pageup 280)
  (:key-pagedown 281)
  (:key-f1 282)
  (:key-f2 283)
  (:key-f3 284)
  (:key-f4 285)
  (:key-f5 286)
  (:key-f6 287)
  (:key-f7 288)
  (:key-f8 289)
  (:key-f9 290)
  (:key-f10 291)
  (:key-f11 292)
  (:key-f12 293)
  (:key-f13 294)
  (:key-f14 295)
  (:key-f15 296)
  (:key-numlock 300)
  (:key-capslock 301)
  (:key-scrollock 302)
  (:key-rshift 303)
  (:key-lshift 304)
  (:key-rctrl 305)
  (:key-lctrl 306)
  (:key-ralt 307)
  (:key-lalt 308)
  (:key-rmeta 309)
  (:key-lmeta 310)
  (:key-lsuper 311)
  (:key-rsuper 312)
  (:key-mode 313)
  (:key-compose 314)
  (:key-help 315)
  (:key-print 316)
  (:key-sysreq 317)
  (:key-break 318)
  (:key-menu 319)
  (:key-power 320)
  (:key-euro 321)
  (:key-undo 322)
  :key-last)

(cffi:defcenum sdl-mod
  (:mod-none #x0000)
  (:mod-lshift #x0001)
  (:mod-rshift #x0002)
  (:mod-lctrl #x0040)
  (:mod-rctrl #x0080)
  (:mod-lalt #x0100)
  (:mod-ralt #x0200)
  (:mod-lmeta #x0400)
  (:mod-rmeta #x0800)
  (:mod-num #x1000)
  (:mod-caps #x2000)
  (:mod-mode #x4000)
  (:mod-reserved #x8000))




;; Resources


(defvar *resources* () "List of currently loaded resources.")

(defstruct image
  (file nil)
  (texture 0 :type u11)                 ; "GL texture id for image."
  (texture-width 0 :type u11) ; "Actual (rounded up to power of two) width of texture."
  (texture-height 0 :type u11) ; "Actual (rounded up to power of two) height of texture."
  (tx2 0 :type single-float)           ; "tx2 = width / texture-width"
  (ty2 0 :type single-float)          ; "ty2 = height / texture-width"
  (height 0 :type u11)            ; "Height of textures visible part."
  (width 0 :type u11))             ; "Width of textures visible part."

(defstruct font
  (file nil)
  (image nil :type (or boolean image))
  (glyphs nil :type (or boolean (simple-vector 256)))
  (height 0 :type u11))

(defstruct music
  (file nil)
  music)

(defstruct sample
  (file nil)
  chunk)


(deftype resource () '(or music sample image font))

(defun resource-p (object)
  (typep object 'resource))



(defgeneric register-resource (resource)
  (:documentation "Add RESOURCE to *RESOURCES*"))

;; NOTE: Does not free the resource if it is held by some other resource.
(defgeneric free-resource (resource)
  (:documentation "Free the RESOURCE and all system resources used by it. Also resets the TAGs related to the resource."))

(defgeneric holdsp (holder resource))



(defmethod holdsp (holder resource)
  nil)

(defun heldp (resource)
  (find-if (lambda (holder) (holdsp holder resource)) *resources*))

(defmethod register-resource (resource)
  (assert (resource-p resource))
  (push resource *resources*)
  resource)



(defmethod free-resource :around (resource)
  (assert (typep resource 'resource))
  (when (and (not (heldp resource)) (find resource *resources*))
    (call-next-method)
    (pal::reset-tags :resource resource)
    (setf *resources* (remove resource *resources*))))



(defmethod free-resource ((resource music))
  (assert (music-music resource))
  (free-music (music-music resource))
  (setf (music-music resource) nil))



(defmethod free-resource ((resource font))
  (assert (font-image resource))
  (let ((image (font-image resource)))
    (setf (font-image resource) nil)
    (free-resource image)))

(defmethod holdsp ((font font) (image image))
  (eq (font-image font) image)) ;; Font resources need to hold the image they are using for the glyphs.



(defmethod free-resource ((resource image))
  (assert (> (image-texture resource) 0))
  (gl-delete-texture (image-texture resource))
  (setf (image-texture resource) 0))



(defmethod free-resource ((resource sample))
  (assert (sample-chunk resource))
  (free-chunk (sample-chunk resource))
  (setf (sample-chunk resource) nil))



(defun free-all-resources ()
  "Free all loaded resources and reset the TAGS"
  (loop while *resources* do
       (free-resource (first *resources*)))
  (assert (null *resources*)))


;; Main SDL functions

(cffi:defcfun ("SDL_Init" init) :int
  (flags :uint))

(cffi:defcfun ("SDL_InitSubSystem" init-subsystem) :int
  (flags :uint))

(cffi:defcfun ("SDL_QuitSubSystem" quit-subsystem) :void
  (flags :uint))

(cffi:defcfun ("SDL_Quit" Quit) :void)

(cffi:defcfun ("SDL_SetVideoMode" set-video-mode) :pointer
  (width :int)
  (height :int)
  (bpp :int)
  (flags :uint))


(cffi:defcfun ("SDL_VideoModeOK" video-mode-ok) :int
  (width :int)
  (height :int)
  (bpp :int)
  (flags :uint))

(cffi:defcfun ("SDL_RWFromFile" rw-from-file) :pointer
  (file :string)
  (mode :string))

(cffi:defcfun ("SDL_GetTicks" get-tick) :uint)

(cffi:defcfun ("SDL_Delay" Delay) :void
  (ms :uint))

(cffi:defcfun ("SDL_GetMouseState" get-mouse-state) :uint8
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("SDL_GetRGBA" get-rgba) :void
  (pixel :uint32)
  (format :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (a :pointer))

(cffi:defcfun ("SDL_GetRelativeMouseState" get-relative-mouse-state) :uint8
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("SDL_WarpMouse" warp-mouse) :void
  (x :uint16)
  (y :uint16))

(cffi:defcfun ("SDL_ShowCursor" show-cursor) :boolean
  (toggle :boolean))

(cffi:defcfun ("SDL_MapRGB" map-rgb) :uint
  (format :pointer)
  (r :uint8)
  (g :uint8)
  (b :uint8))

(cffi:defcfun ("SDL_FreeSurface" free-surface) :void
  (surface :pointer))

(cffi:defcfun ("SDL_WM_SetCaption" set-caption) :void
  (title :string)
  (icon :string))

(cffi:defcfun ("IMG_Load" load-image) :pointer
  (string :string))

(cffi:defcfun ("SDL_PollEvent" poll-event) :boolean
  (event :pointer))




;; Audio

(cffi:defcfun ("Mix_OpenAudio" open-audio) :int
  (frequency :int)
  (format :uint16)
  (channels :int)
  (chunksize :int))

(cffi:defcfun ("Mix_CloseAudio" close-audio) :void)

(cffi:defcfun ("Mix_HaltChannel" halt-channel) :int
  (channel :int))

(cffi:defcfun ("Mix_FreeChunk" free-chunk) :void
  (chunk :pointer))

(cffi:defcfun ("Mix_LoadWAV_RW" load-wav-rw) :pointer
  (io :pointer) (int :int))

(defun load-wav (file)
  (load-wav-rw (rw-from-file file "rb") 1))

(cffi:defcfun ("Mix_VolumeChunk" volume-chunk) :int
  (chunk :pointer) (volume :int))

(cffi:defcfun ("Mix_SetPosition" set-position) :int
  (channel :int) (angle :int16) (distance :uint8))

(cffi:defcfun ("Mix_QuickLoad_RAW" quickload-raw) :pointer
  (uint8-ptr :pointer) (length :uint32))

(cffi:defcfun ("Mix_PlayChannelTimed" play-channel-timed) :int
  (channel :int) (chunk :pointer) (loops :int) (ticks :int))

(defun play-channel (channel chunk loops)
  (play-channel-timed channel chunk loops -1))

(cffi:defcfun ("Mix_HaltMusic" halt-music) :int)

(cffi:defcfun ("Mix_FadeOutMusic" fade-out-music) :int
  (fade :int))

(cffi:defcfun ("Mix_LoadMUS" load-music) :pointer
  (file :string))

(cffi:defcfun ("Mix_FadeInMusic" fade-in-music) :int
  (music :pointer) (loops :int) (fade :int))

(cffi:defcfun ("Mix_PlayMusic" play-music) :int
  (music :pointer) (loops :int))

(cffi:defcfun ("Mix_PauseMusic" pause-music) :void)

(cffi:defcfun ("Mix_PausedMusic" paused-music) :int)

(cffi:defcfun ("Mix_ResumeMusic" resume-music) :void)

(cffi:defcfun ("Mix_RewindMusic" rewind-music) :void)

(cffi:defcfun ("Mix_FreeMusic" free-music) :void
  (music :pointer))

(cffi:defcfun ("Mix_VolumeMusic" volume-music) :int
  (volume :int))


;; OpenGL

(defconstant +gl-red-size+ 0)
(defconstant +gl-green-size+ 1)
(defconstant +gl-blue-size+ 2)
(defconstant +gl-alpha-size+ 3)
(defconstant +gl-buffer-size+ 4)
(defconstant +gl-doublebuffer+ 5)
(defconstant +gl-depth-size+ 6)
(defconstant +gl-stencil-size+ 7)
(defconstant +gl-color-buffer-bit+ #x4000)
(defconstant +gl-texture-2d+ #xDE1)
(defconstant +gl-depth-test+ #xB71)
(defconstant +gl-cull-face-test+ #xB44)
(defconstant +gl-modelview+ #x1700)
(defconstant +gl-projection+ #x1701)
(defconstant +gl-lines+ #x1)
(defconstant +gl-line-loop+ #x2)
(defconstant +gl-polygon+ #x9)
(defconstant +gl-quads+ #x7)
(defconstant +gl-PACK-ALIGNMENT+ #xD05)
(defconstant +gl-blend+ #xBE2)
(defconstant +gl-src-alpha+ #x302)
(defconstant +gl-dst-alpha+ #x304)
(defconstant +gl-one+ 1)
(defconstant +gl-flat+ #x1d00)
(defconstant +gl-polygon-smooth+ #xb41)
(defconstant +gl-zero+ 0)
(defconstant +gl-points+ 0)
(defconstant +gl-ONE-MINUS-DST-ALPHA+ #x305)
(defconstant +gl-ONE-MINUS-DST-COLOR+ #x307)
(defconstant +gl-MAX-TEXTURE-SIZE+ #xD33)
(defconstant +gl-ONE-MINUS-SRC-ALPHA+ #x303)
(defconstant +gl-ONE-MINUS-SRC-COLOR+ #x301)
(defconstant +gl-texture-mag-filter+ #x2800)
(defconstant +gl-texture-min-filter+ #x2801)
(defconstant +gl-linear+ #x2601)
(defconstant +gl-rgba+ #x1908)
(defconstant +gl-compile+ #x1300)
(defconstant +gl-rgb+ #x1907)
(defconstant +gl-scissor-test+ #xC11)
(defconstant +gl-unsigned-byte+ #x1401)
(defconstant +gl-vendor+ #x1F00)
(defconstant +gl-renderer+ #x1F01)
(defconstant +gl-version+ #x1F02)
(defconstant +gl-extensions+ #x1F03)
(defconstant +gl-ALPHA-TEST+ #xBC0)
(defconstant +gl-ALPHA-TEST-FUNC+ #xBC1)
(defconstant +gl-GREATER+ #x204)
(defconstant +gl-CURRENT-BIT+ #x1)
(defconstant +gl-DEPTH-BUFFER-BIT+ #x100)
(defconstant +gl-ENABLE-BIT+ #x2000)
(defconstant +gl-LINE-BIT+ #x4)
(defconstant +gl-smooth+ #x1D01)
(defconstant +gl-LINE-SMOOTH+ #xB20)
(defconstant +gl-NEAREST+ #x2600)
(defconstant +gl-point-smooth+ #xB10)
(defconstant +gl-point+ #x0)

(cffi:defcfun ("glFlush" gl-flush) :void)

(cffi:defcfun ("glAlphaFunc" gl-alpha-func) :void
  (func :int)
  (ref :float))

(cffi:defcfun ("glBlendFunc" gl-blendfunc) :void
  (src :int)
  (dst :int))

(cffi:defcfun ("SDL_GL_SetAttribute" gl-set-attribute) :int
  (attribute :int) (value :int))

(cffi:defcfun ("SDL_GL_SwapBuffers" gl-swap-buffers) :void)

(cffi:defcfun ("glClear" gl-clear) :void
  (bits :int))

(cffi:defcfun ("glEnable" gl-enable) :void
  (flag :int))

(cffi:defcfun ("glDisable" gl-disable) :void
  (flag :int))

(cffi:defcfun ("glLoadIdentity" gl-load-identity) :void)

(cffi:defcfun ("glMatrixMode" gl-matrix-mode) :void
  (mode :int))

(cffi:defcfun ("glClearColor" gl-clear-color) :void
  (r :float)
  (g :float)
  (b :float)
  (a :float))

(cffi:defcfun ("glOrtho" gl-ortho) :void
  (left :double)
  (right :double)
  (bottom :double)
  (top :double)
  (near :double)
  (far :double))

(cffi:defcfun ("glViewport" gl-viewport) :void
  (x :int)
  (y :int)
  (w :int)
  (h :int))


(cffi:defcfun ("glBegin" gl-begin) :void
  (mode :int))

(cffi:defcfun ("glEnd" gl-end) :void)

(cffi:defcfun ("glVertex2f" gl-vertex2f) :void
  (x :float)
  (y :float))

(cffi:defcfun ("glVertex2i" gl-vertex2i) :void
  (x :int)
  (y :int))

(cffi:defcfun ("glColor4ub" gl-color4ub) :void
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(cffi:defcfun ("glTexParameteri" gl-tex-parameteri) :void
  (target :int)
  (pname :int)
  (param :int))

(cffi:defcfun ("glTexCoord2f" gl-tex-coord2f) :void
  (s :float)
  (t* :float))

(cffi:defcfun ("glGenTextures" gl-gen-textures) :void
  (n :int)
  (textures :pointer))

(cffi:defcfun ("glBindTexture" gl-bind-texture) :void
  (target :int)
  (handle :uint))

(cffi:defcfun ("glTexImage2D" gl-teximage2D) :void
  (target :int)
  (level :int)
  (internal-format :int)
  (width :int)
  (height :int)
  (border :int)
  (format :int)
  (type :int)
  (data :pointer))

(cffi:defcfun ("glRotatef" gl-rotatef) :void
  (theta :float)
  (x :float)
  (y :float)
  (z :float))

(cffi:defcfun ("glTranslatef" gl-translatef) :void
  (x :float)
  (y :float)
  (z :float))

(cffi:defcfun ("glScalef" gl-scalef) :void
  (x :float)
  (y :float)
  (z :float))

(cffi:defcfun ("glPushMatrix" gl-push-matrix) :void)
(cffi:defcfun ("glPopMatrix" gl-pop-matrix) :void)

(cffi:defcfun ("glDeleteTextures" gl-delete-textures) :void
  (n :int)
  (textures :pointer))

(defun gl-delete-texture (texture)
  (let ((id (cffi:foreign-alloc :uint :count 1 :initial-element texture)))
    (gl-delete-textures 1 id)
    (cffi:foreign-free id)))

(cffi:defcfun ("glGetString" gl-get-string) :string
  (flag :int))

(cffi:defcfun ("glPushAttrib" gl-push-attrib) :void
  (mask :int))

(cffi:defcfun ("glPopAttrib" gl-pop-attrib) :void)

(cffi:defcfun ("glLineWidth" gl-line-width) :void
  (width :float))

(cffi:defcfun ("glPointSize" gl-point-size) :void
  (size :float))

(cffi:defcfun ("glShadeModel" gl-shade-model) :void
  (mode :int))

(cffi:defcfun ("glRectf" gl-rectf) :void
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float))

(cffi:defcfun ("glScissor" gl-scissor) :void
  (left :int)
  (bottom :int)
  (width :int)
  (height :int))

(cffi:defcfun ("glGetError" gl-get-error) :int)

(cffi:defcfun ("glGetIntegerv" %gl-get-integer) :void
  (value :int)
  (data :pointer))

(defun gl-get-integer (value)
  (cffi:with-foreign-object (data :int)
    (%gl-get-integer value data)
    (cffi:mem-ref data :int)))

(cffi:defcfun ("glReadPixels" gl-read-pixels) :void
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (format :int)
  (type :int)
  (data :pointer))

(cffi:defcfun ("glPixelStorei" gl-pixel-store) :void
  (pack :int)
  (value :int))


;; Used to get the application data folder.
#+win32 (cffi:defcfun "SHGetFolderPathA" :int (owner :pointer) (folder :int) (handle :pointer) (flags :int) (path :pointer))

#+win32 (defun get-application-folder ()
          (cffi:with-foreign-object (path :char 4096)
            (shgetfolderpatha (cffi:null-pointer) #x001a (cffi:null-pointer) 0 path)
            (concatenate 'string (cffi:foreign-string-to-lisp path) "/")))


;; Used to allocate zeroed memory.
(cffi:defcfun "calloc" :pointer (nelem :uint) (elsize :uint))

;; Can we just use cffi:foreign-free? Just in case...
(cffi:defcfun "free" :void (ptr :pointer))