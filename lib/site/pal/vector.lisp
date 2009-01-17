(declaim (optimize (speed 3)
                   (safety 1)))

(in-package :pal)


(deftype component () 'single-float)

(defstruct (vec (:conc-name v))
  (x 0 :type component) (y 0 :type component))


(declaim (inline component))
(defunct component (x)
    (number x)
  (coerce x 'component))

(declaim (inline v))
(defunct v (x y)
    (component x component y)
  (make-vec :x x :y y))

(declaim (inline vf))
(defun vf (x y)
  (declare (type component x) (type component y))
  (make-vec :x x :y y))



(declaim (inline rad))
(defunct rad (degrees)
    (component degrees)
  (* (/ pi 180) degrees))

(declaim (inline deg))
(defunct deg (radians)
    (component radians)
  (* (/ 180 pi) radians))



(declaim (inline angle-v))
(defunct angle-v (angle)
    (component angle)
  (v (sin (rad angle)) (- (cos (rad angle)))))

(declaim (inline v-angle))
(defunct v-angle (vec)
    (vec vec)
  (mod (deg (atan (vx vec)
                  (if (zerop (vy vec))
                      least-negative-short-float
                      (- (vy vec))) ))
       360))

(defunct v-random (length)
    (number length)
  (v* (angle-v (random 360.0)) length))

(declaim (inline v-round))
(defunct v-round (v)
    (vec v)
  (v (round (vx v)) (round (vy v))))

(declaim (inline v-floor))
(defunct v-floor (v)
    (vec v)
  (v (floor (vx v)) (floor (vy v))))


(declaim (inline v=))
(defunct v= (a b)
    (vec a vec b)
  (and (= (vx a) (vx b))
       (= (vy a) (vy b))))

(declaim (inline v+!))
(defunct v+! (a b)
    (vec a vec b)
  (setf (vx a) (+ (vx a) (vx b)))
  (setf (vy a) (+ (vy a) (vy b)))
  nil)

(declaim (inline v+))
(defunct v+ (a b)
    (vec a vec b)
  (vf (+ (vx a) (vx b))
      (+ (vy a) (vy b))))


(declaim (inline v-))
(defunct v- (a b)
    (vec a vec b)
  (vf (- (vx a) (vx b))
      (- (vy a) (vy b))))

(declaim (inline v-!))
(defunct v-! (a b)
    (vec a vec b)
  (setf (vx a) (- (vx a) (vx b)))
  (setf (vy a) (- (vy a) (vy b)))
  nil)


(declaim (inline v*!))
(defunct v*! (v m)
    (component m)
  (setf (vx v) (* (vx v) m))
  (setf (vy v) (* (vy v) m))
  nil)

(declaim (inline v*))
(defunct v* (v m)
    (vec v component m)
  (vf (* (vx v) m)
      (* (vy v) m)))


(declaim (inline v/))
(defunct v/ (v d)
    (vec v component d)
  (vf (/ (vx v) d)
      (/ (vy v) d)))

(declaim (inline v/!))
(defunct v/! (v d)
    (vec v component d)
  (setf (vx v) (/ (vx v) d))
  (setf (vy v) (/ (vy v) d))
  nil)

(declaim (inline v-max))
(defunct v-max (a b)
    (vec a vec b)
  (if (< (v-magnitude a) (v-magnitude b))
      b a))


(declaim (inline v-min))
(defunct v-min (a b)
    (vec a vec b)
  (if (< (v-magnitude a) (v-magnitude b))
      a b))


(defunct v-rotate (v a)
    (vec v component a)
  (let ((a (rad a)))
    (v (- (* (cos a) (vx v))
          (* (sin a) (vy v)))
       (+ (* (sin a) (vx v))
          (* (cos a) (vy v))))))

(declaim (inline v-dot))
(defunct v-dot (a b)
    (vec a vec b)
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))))


(declaim (inline v-magnitude))
(defunct v-magnitude (v)
    (vec v)
  (the component (sqrt (the component
                         (+ (expt (vx v) 2)
                            (expt (vy v) 2))))))

(defunct v-normalize (v)
    (vec v)
  (let ((m (v-magnitude v)))
    (if (/= m 0f0)
        (vf (/ (vx v) m)
            (/ (vy v) m))
        (vf 0f0 0f0))))

(defunct v-direction (from-vector to-vector)
    (vec from-vector vec to-vector)
  (v-normalize (v- to-vector from-vector)))

(defunct v-distance (v1 v2)
    (vec v1 vec v2)
  (v-magnitude (v- v1 v2)))


(defunct v-truncate (v l)
    (vec v component l)
  (v* (v-normalize v) l))




(defunct closest-point-to-line (a b p)
    (vec a vec b vec p)
  (let* ((dir (v- b a))
         (diff (v- p a))
         (len (v-dot dir dir)))
    (if (< len .001)
        nil
        (let ((u (/ (v-dot dir diff) len)))
          (if (> u 0)
              (if (< u 1)
                  (v+ a (v* dir u))
                  b)
              a)))))

(defunct point-in-line-p (a b p)
    (vec a vec b vec p)
  (let ((d (v-direction a b)))
    (if (< (abs (+ (v-dot d (v-direction a p))
                   (v-dot d (v-direction b p)))) .00001)
        t nil)))

(defunct lines-intersection (la1 la2 lb1 lb2)
    (vec la1 vec la2 vec lb1 vec lb2)
  (let ((x1 (vx la1))
        (y1 (vy la1))
        (x2 (vx la2))
        (y2 (vy la2))
        (x3 (vx lb1))
        (y3 (vy lb1))
        (x4 (vx lb2))
        (y4 (vy lb2)))
    (let* ((a1 (- y2 y1))
           (b1 (- x1 x2))
           (c1 (- (* x2 y1) (* x1 y2)))
           (a2 (- y4 y3))
           (b2 (- x3 x4))
           (c2 (- (* x4 y3) (* x3 y4)))
           (denom (- (* a1 b2) (* a2 b1))))
      (if (zerop denom)
          nil
          (let ((p (vf (/ (- (* b1 c2) (* b2 c1)) denom)
                       (/ (- (* a2 c1) (* a1 c2)) denom))))
            (if (and (point-in-line-p la1 la2 p)
                     (point-in-line-p lb1 lb2 p))
                p
                nil))))))

(defunct circle-line-intersection (a b co r)
    (vec a vec b vec co component r)
  (let ((cp (closest-point-to-line a b co)))
    (if cp
        (if (<= (v-distance co cp) r)
            cp
            nil)
        nil)))

(defunct distance-from-line (a b p)
    (vec a vec b vec p)
  (let ((cp (closest-point-to-line a b p)))
    (if cp
        (v-distance cp p)
        nil)))

(defunct point-inside-rectangle-p (topleft width height point)
    (vec topleft vec point component width component height)
  (let* ((x1 (vx topleft))
         (y1 (vy topleft))
         (x2 (+ x1 width))
         (y2 (+ y1 height))
         (x (vx point))
         (y (vy point)))
    (if (and (> x x1) (< x x2)
             (> y y1) (< y y2))
        t nil)))


(defunct rectangles-overlap-p (a a-width a-height b b-width b-height)
    (vec a component a-width component a-height vec b component b-width component b-height)
  (let ((ax (vx a))
        (ay (vy a))
        (bx (vx b))
        (by (vy b)))
    (not (or (> ax (+ bx b-width))
             (< (+ ax a-width) bx)
             (> ay (+ by b-height))
             (< (+ ay a-height) by)))))


(declaim (inline point-inside-circle-p))
(defunct point-inside-circle-p (co r p)
    (vec co vec p component r)
  (<= (v-distance co p) r))

(declaim (inline circles-overlap-p))
(defunct circles-overlap-p (c1 r1 c2 r2)
    (vec c1 vec c2 component r1 component r2)
  (<= (v-distance c1 c2) (+ r2 r1)))