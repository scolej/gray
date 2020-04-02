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

(define (ray-at r t)
  (v+ (ray-origin r)
      (v* (ray-dir r) t)))

(define (lerp a b x)
  (+ (* x b)
     (* (- 1 x) a)))

(define (normalize a b x)
  (/ (- x a) (- b a)))

(define (rescale a b c d x)
  "Linearly interpolate X, which is in the range A through B, into the
range C through D."
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

(define (bg ray)
  (let ((v (vec3-y (ray-dir ray))))
    (list (rescale -1 1 0.5 0.9 v)
          (rescale -1 1 0.5 0.9 v)
          (rescale -1 1 0.9 1.0 v))))

(define (reflect ray pos normal)
  "Make a new ray, reflected perfectly by NORMAL at POS."
  (let* ((d0 (ray-dir ray))
         (d1 (v+ d0 (v* normal (* -2 (vdot normal d0))))))
    (make-ray pos d1 (1+ (ray-bounces ray)))))

;; FIXME probably not technically properly random
;; "corner" values more likely than "face" values
(define (random-unit-sphere)
  (vunit (make-vec3 (- (random 2.0) 1)
                    (- (random 2.0) 1)
                    (- (random 2.0) 1))))

(define (diffuse-reflect ray pos normal)
  "Make a new ray, reflected diffusely by NORMAL at POS."
  (make-ray pos
            (vunit (v+ normal (random-unit-sphere)))
            (1+ (ray-bounces ray))))

(define (hit-sphere centre radius ray)
  "Determine if RAY hits a sphere at CENTRE with RADIUS.
Return #f if it misses, otherwise how far along RAY you need to go to
get to the first intersection with the sphere's surface."
  (let* ((oc (v- (ray-origin ray) centre))
         (dir (ray-dir ray))
         (a (vdot dir dir))
         (b (* 2.0 (vdot oc dir)))
         (c (- (vdot oc oc) (* radius radius)))
         (descr (- (* b b) (* 4 a c))))
    (if (> descr 0)
        (/ (+ b (sqrt descr)) (* -2 a))
        #f)))

(define (sphere centre radius)
  (lambda (ray world)
    (let ((t (hit-sphere centre radius ray)))
      (if (or (not t) (< t 0))
          'missed
          (cons
           t
           (lambda ()
             (let* ((hit-point (ray-at ray t))
                    (normal (vunit (v- hit-point centre))))
               (attenuate
                0.8
                (blend '(1 0 0)
                       (trace world (reflect ray hit-point normal))
                       0.5)))))))))

;; FIXME surely I exist in the standard libraries
(define (repeat n f)
  (if (= n 0) '()
      (cons (f) (repeat (1- n) f))))

(define (inf-plane y)
  "Infinite horizontal green plane offset vertically by Y."
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

(define world
  (list (sphere (make-vec3 -3.0  2.0 -5.3)
                0.7)
        (sphere (make-vec3  0.0  0.1 -3.0)
                0.7)
        (sphere (make-vec3  0.7 -0.8 -1.8)
                0.7)
        (sphere (make-vec3  -0.8 -10.74 -1.6)
                10.0)
        (inf-plane -0.8)))

(define (trace world ray)
  "Each entry in WORLD represents an object. The entry should be a
function of RAY and WORLD which returns one of:
 - The symbol 'missed to indicate the ray does not hit the object.
 - A pair (T . F) where T is the distance travelled along RAY before
hitting the object. F is a function of no arguments which determines
the colour seen along RAY."
  (let ((objects (map (lambda (o) (o ray world)) world)))
    (cond
     ((> (ray-bounces ray) 5) '(0 0 0))
     ((every (lambda (s) (eq? s 'missed)) objects) (bg ray))
     (else ((cdr (car (sort (remove (lambda (v) (not (pair? v)))
                                    objects)
                            (lambda (a b) (< (car a) (car b)))))))))))

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

(define (img-fun x y)
  (let* ((cam-pos (make-vec3 0 0 0.5))
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

(define (make-image f)
  (call-with-output-file "img.ppm"
    (lambda (s)
      (write-ppm s
                 (inexact->exact (round (* f 640)))
                 (inexact->exact (round (* f 480))) img-fun))))

;; (make-image 0.3)
(make-image 1.0)
