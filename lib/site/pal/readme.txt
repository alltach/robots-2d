
Linux gfx card problems
--------------------------------------------------------------------------------

(Edit: Update to Ubuntu 7.10 seems to have fixed my problems with X550.)

It seems that some people (yours truly included, running Ubuntu 7.04 with ATI
X550 and the OSS drivers) are having problems under Linux when trying to run
PAL applications several times in the same Lisp session. I did some testing and
it seems to be a problem in some graphics cards drivers. Of course it is
possible that there is a bug in PAL, but so far I haven't find it.

Running the following function twice after PAL is loaded should trigger the bug,
if present on your system:

-----------

(defun test-open-close ()
  (pal-ffi::init pal-ffi::+init-video+)
  (pal-ffi::gl-set-attribute pal-ffi::+gl-depth-size+ 0)
  (pal-ffi::gl-set-attribute pal-ffi::+gl-doublebuffer+ 1)
  (when (cffi:null-pointer-p (pal-ffi::set-video-mode
                              800
                              600
                              0
                              (logior pal-ffi::+gl-doublebuffer+ pal-ffi::+opengl+)))
    (error "PAL failed to obtain SDL surface"))
  (pal-ffi::quit))

-----------

This happens on my computer with both SBCL and CLisp, but not with an equivalent
C program or any of the Windows CLs that I have tried. So far I have no idea
what is causing this but if anyone has any clues or more info I'd appreciate
sharing it.

Since this kind of bug causes problems when developing in an incremental, "live"
environment like CL here are some suboptimal workarounds:

- The bug doesn't seem to appear with all drivers/gfx cards. Especially running
X11 without HW acceleration should be safe.

- Never call CLOSE-PAL. I haven't tested it much, but it should be possible to
just call OPEN-PAL when starting your lisp session and never use CLOSE-PAL or
WITH-PAL (which eventually calls CLOSE-PAL). Of course this means that some
parameters like window size can't be changed after initialisation.

- Never return from WITH-PAL. Run your main loop in a separate thread and
install condition handlers that just restart your main loop without closing
down PAL. That way you can incrementally change your functions/classes etc.
while your app is running. I might actually add this as an option to WITH-PAL.

All in all this bug mostly has effect only while developing, applications that
don't need to open/close PAL several times should work fine.

-- tomppa




About performance
--------------------------------------------------------------------------------
Few notes about how to get the maximum graphics performance from PAL:

First, if you don't notice any problems there is no need to worry about  
performance. Using OpenGL for 2d graphics is likely to be very fast, even  
when naively implemented and running on low-end hardware.


Functions like draw-circle, -line and -polygon are quite slow. Normally it  
shouldn't be problem but if you want to do complex vector graphics it  
could. This is mostly a design issuea since PAL is more oriented towards  
bitmap graphics, if you need faster polygon primitives let me know the  
details and I'll see what I can do.

Internally draw-image/draw-image* works by "chaining" the draw operations  
and as long as the chain is not cut performance is very good. If the chain  
is repeatedly cut you will get lousy performance.

The chain is cut when:

- You call any graphics function except draw-image or draw-image*.
- You use any graphics state altering functions or macros (rotate, scale,  
set-blend-mode, with-transformation etc.) except set-blend-color.
- You draw a different image than with the previous draw-image calls.  
Internally PAL keeps count of the "current" image and whenever it changes  
the chain gets cut.
- You use the :angle or :scale keywords in draw-image. That maybe fixed in  
the future. (Also the alignment keywords cut the chain, due to my  
laziness. I'll fix that soon.)

It's okay to have rotations and image changes but to get maximum  
performance you need to make sure they don't regularly cut the chain.
So if you are only allowed to draw the same image again and again how you  
get anything interesting on the screen? By tiling your graphics in one big  
image and using the draw-image* you can avoid the need to change image and  
in some cases you can use set-blend-color to change the color of image.
At some point I'm going to add a mechanism for cutting images to tiles  
which then can be used interchangebly with regular images, that should  
make avoiding image changes much easier.


About the examples/

- teddy.lisp is an especially bad example of chaining. Since the teddies  
all have the same image drawing them would be very fast if not
a) when drawing the shadows with-transformation gets repeatedly called. It  
would be better to translate the shadow position manually
b) the teddies need to be rotated.

- hares.lisp works suprisingly well altough it uses rotations and scaling.  
It should be very fast if these wouldn't cut the chain :(

Again, if you don't have any perfomance problems just ignore what I just  
wrote :)


-- 
tomppa
