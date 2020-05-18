(use-modules
 (srfi srfi-1)
 (srfi srfi-9)
 (ice-9 format)
 (vector))

(define-record-type ray
  (make-ray origin direction bounces)
  ray?
  (origin ray-origin)
  (direction ray-dir)
  (bounces ray-bounces))

;; Returns the position after travelling a distance of T along ray R.
(define (ray-at r t)
  (v+ (ray-origin r)
      (v* (ray-dir r) t)))

(define (lerp a b x)
  (+ (* x b)
     (* (- 1 x) a)))

;; Returns a number between 0 and 1 representing how far X is between
;; A and B.
(define (normalize a b x)
  (/ (- x a) (- b a)))

;; Linearly interpolate X, which is in the range A through B, into the
;; range C through D.
(define (rescale a b c d x)
  (lerp c d (normalize a b x)))

(define (avg . xs)
  (let ((l (length xs)))
    (if (= 0 l) #f
        (/ (apply + xs) l))))

(define (avgl . ls)
  (apply map avg ls))

(define (blend as bs x)
  (map (lambda (a b) (lerp a b x)) as bs))

(define (attenuate z xs)
  (map (lambda (x) (lerp 0 x z)) xs))

(define (clip min max x)
  (cond ((> x max) max)
        ((< x min) min)
        (#t x)))

;; Direction in which sun is shining.
(define sun-dir
  (vunit (make-vec3 -1.0 1.0 0.0)))

;; When the ray doesn't hit anything, use this for a background colour.
(define (bg ray)
  (let ((v (rescale -1 1 0 1
                    (vdot (ray-dir ray)
                          sun-dir))))
    (blend '(0.2 0.2 0.4) '(1 1 1) v)))

;; Reflect RAY into a new ray, reflected perfectly by NORMAL at POS.
(define (reflect ray pos normal)
  (let* ((d0 (ray-dir ray))
         (d1 (v+ d0 (v* normal (* -2 (vdot normal d0))))))
    (make-ray pos d1 (1+ (ray-bounces ray)))))

;; Makes a random unit length vector, pointing in any direction.
;; FIXME probably not technically properly random
;; "corner" values more likely than "face" values
(define (random-unit-sphere)
  (vunit (make-vec3 (- (random 2.0) 1)
                    (- (random 2.0) 1)
                    (- (random 2.0) 1))))

;; Make a new ray, reflected diffusely by NORMAL at POS.
(define (diffuse-reflect ray pos normal)
  (make-ray pos
            (vunit (v+ normal (random-unit-sphere)))
            (1+ (ray-bounces ray))))

;; Determine if RAY hits a sphere at CENTRE with RADIUS.
;; Return #f if it misses, otherwise how far along RAY you need to go to
;; get to the first intersection with the sphere's surface.
(define (hit-sphere centre radius ray)
  (let* ((oc (v- (ray-origin ray) centre))
         (dir (ray-dir ray))
         (a (vdot dir dir))
         (b (* 2.0 (vdot oc dir)))
         (c (- (vdot oc oc) (* radius radius)))
         (descr (- (* b b) (* 4 a c))))
    (if (> descr 0)
        (/ (+ b (sqrt descr)) (* -2 a))
        #f)))

;; Material which is perfectly reflective. The ray bounces straight
;; off, all colour comes from wherever the ray goes.
(define (perfectly-reflective ray t hit-point normal)
  (lambda ()
    (trace world (reflect ray hit-point normal))))

;; Imperfect reflection. Some colour from this material, some colour
;; from randomly refelcted rays.
(define (diffuse-colour r g b att)
  (lambda (ray t hit-point normal)
    (lambda ()
      (let ((ca (list r g b))
            (cb (apply
                 avgl (repeat
                       10
                       (lambda ()
                         (trace world
                                (diffuse-reflect ray hit-point normal)))))))
        (attenuate att (blend ca cb 0.5))))))

;; A different colour every time we hit the surface!
(define (totally-random ray t hit-point normal)
  (lambda ()
    (list (random 1.0)
          (random 1.0)
          (random 1.0))))

(define (totally-black ray t hit-point normal)
  (lambda () '(0 0 0)))

;; Make a sphere at CENTRE with RADIUS and MATERIAL.
;; CENTRE and RADIUS are vectors.
;;
;; MATERIAL should be a procedure accepting:
;; - the incident ray
;; - the distance along the ray at which the surface is to be found
;; - the hit point
;; - the surface normal
;; which returns a thunk to determine the surface colour.
(define (sphere centre radius material)
  (lambda (ray world)
    (let ((t (hit-sphere centre radius ray)))
      (if (or (not t) (< t 0))
          'missed
          (cons
           t
           (let* ((hit-point (ray-at ray t))
                  (normal (vunit (v- hit-point centre))))
             (material ray t hit-point normal)))))))

;; FIXME surely I exist in the standard libraries
(define (repeat n f)
  (if (= n 0) '()
      (cons (f) (repeat (1- n) f))))

;; Infinite horizontal green plane offset vertically by Y.
(define (inf-plane y)
  (let ((n (make-vec3 0 1 0))
        (po (make-vec3 0 y 0)))
    (lambda (ray world)
      (let ((denom (vdot n (ray-dir ray))))
        (if (>= denom 0)
            'missed
            (let* ((po->o (v- (ray-origin ray) po))
                   (t (/ (vdot n po->o) denom -1))
                   (hit-point (ray-at ray t)))
              (if (< t 0)
                  'missed
                  (cons
                   t
                   (lambda ()
                     (attenuate
                      0.8
                      (blend
                       ;; Blend towards sky as we get further away
                       (blend '(0.3 0.6 0.3) '(0.7 0.8 0.9) (- 1 (/ 1 (+ (* 0.3 t) 1))))
                       ;; Send out random rays and average them
                       (apply
                        avgl (repeat
                              10
                              (lambda ()
                                (trace world
                                       (diffuse-reflect ray hit-point n)))))
                       0.5)))))))))))

;; Maximum ray bounces before giving up tracing a ray.
(define max-bounces 3)

;; Each entry in WORLD represents an object. The entry should be a
;; function of RAY and WORLD which returns one of:
;; - The symbol 'missed to indicate the ray does not hit the object.
;; - A pair (T . F) where T is the distance travelled along RAY before
;; hitting the object. F is a function of no arguments which determines
;; the colour seen along RAY.
(define (trace world ray)
  (if (> (ray-bounces ray) max-bounces) '(0 0 0)
      (let ((objects (map (lambda (o) (o ray world)) world)))
        (if (every (lambda (s) (eq? s 'missed)) objects) (bg ray)
            (let* (;; Objects which we intersect at some point on the ray
                   (hit (remove (lambda (v) (not (pair? v))) objects))
                   ;; Intersected objects, sorted by distance along ray
                   (s (lambda (a b) (< (car a) (car b))))
                   ;; The first intersected object
                   (m (cdr (car (sort hit s)))))
              (m))))))

(define (write-ppm s w h f)
  (display "P3" s) (newline s)
  (format s "~d ~d" w h) (newline s)
  (display "255" s) (newline s)
  (let ((aspect (/ w h)))
    (do ((y 0 (1+ y))) ((= y h))
      ;; Progress indicator
      (format #t "~d " (inexact->exact (round (/ y h 0.01))))
      (do ((x 0 (1+ x))) ((= x w))
        (display
         (string-join
          (map
           ;; Output each colour component
           (lambda (c)
             (format #f "~d"
                     (inexact->exact (round (lerp 0 255 c)))))
           (apply avgl
                  ;; Sample points within a pixel
                  (map
                   (lambda (p)
                     (f (rescale 0 w
                                 (* -1 aspect) aspect
                                 (+ x (car p)))
                        (rescale 0 h
                                 1 -1
                                 (+ y (cdr p)))))
                   '((0.25 . 0.25)
                     (0.75 . 0.25)
                     (0.25 . 0.75)
                     (0.75 . 0.75)))))
          " ") s)
        ;; FIXME trail space
        (display " " s))
      (newline s)))
  (format #t "Done!\n"))

;; FIXME really?
(define pi 3.1415926353)
(define (deg->rad d) (* (/ d 180.0) pi))

;; The world to render.
(define world
  (list
   (sphere (make-vec3  0.0 1.0 -1.0) 0.7 perfectly-reflective)
   (sphere (make-vec3 -1.3 1.1 -1.0) 0.4 (diffuse-colour 1 0 0 0.8))
   (sphere (make-vec3 -0.7 0.7  0.0) 0.4 totally-black)
   (sphere (make-vec3  1.3 1.1 -1.0) 0.4 (diffuse-colour 0 0 1 0.8))
   (sphere (make-vec3  0.7 0.7  0.0) 0.4 totally-random)
   (inf-plane 0.0)))

;; Trace rays through a pixel coordinate and determine the colour.
(define (img-fun x y)
  (let* ((cam-pos (make-vec3 0.0 1.0 2.0))
         (fov (deg->rad 60))
         (theta (* x (/ fov 2)))
         (phi (* y (/ fov 2)))
         (xx (* (cos phi) (sin theta)))
         (zz (* -1 (cos phi) (cos theta)))
         (yy (sin phi)))
    (trace world (make-ray
                  cam-pos
                  (make-vec3 xx yy zz)
                  0))))

;; Make an image, with dimensions scaled by the factor F.
(define (make-image f)
  (call-with-output-file "img.ppm"
    (lambda (s)
      (write-ppm s
                 (inexact->exact (round (* f 640)))
                 (inexact->exact (round (* f 480))) img-fun))))

(make-image 0.1)
