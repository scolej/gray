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

(define (clip min max x)
  (cond ((> x max) max)
        ((< x min) min)
        (#t x)))

(define (bg ray)
  (let ((v (vec3-y (ray-dir ray))))
    (list (rescale -1 1 0.5 0.9 v)
          (rescale -1 1 0.5 0.9 v)
          (rescale -1 1 0.9 1.0 v))))

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
    (let ((hit (hit-sphere centre radius ray)))
      (if (not hit) #f
          (cons
           hit
           (lambda ()
             (let* ((hit-point (ray-at ray hit))
                    (normal (vunit (v- hit-point centre))))
               (blend '(1 0 0)
                      (trace world
                             (make-ray hit-point
                                       normal
                                       (1+ (ray-bounces ray))))
                      0.9))))))))

(define (inf-plane y)
  "Infinite horizontal green plane offset vertically by Y."
  (let ((n (make-vec3 0 1 0))
        (po (make-vec3 0 y 0)))
    (lambda (ray world)
      (let ((denom (vdot n (ray-dir ray))))
        (if (>= denom 0) #f
            (let* ((po->o (v- (ray-origin ray) po))
                   (t (/ (vdot n po->o) denom -1))
                   (hit-point (ray-at ray t)))
              (cons t
                    (lambda ()
                      (blend
                       ;; Blend towards sky as we get further away
                       (blend '(0.3 0.6 0.3) '(0.7 0.8 0.9)
                              (- 1 (/ 1 (+ (* 0.3 t) 1))))
                       (trace world
                              (make-ray hit-point
                                        (make-vec3 0 1 0)
                                        (1+ (ray-bounces ray))))
                       0.5)
                      ))))))))

(define world
  (list (sphere (make-vec3 -1 0 -2.3) 0.7)
        (sphere (make-vec3 0 0 -2.0) 0.7)
        (sphere (make-vec3 1 0 -1.8) 0.7)
        (inf-plane -0.5)))

(define (trace world ray)
  "Each entry in WORLD represents an object. The entry should be a
function of RAY and WORLD which returns either #f to indicate the ray
does not hit the object or a pair (T . F). T is the distance travelled
along RAY before hitting the object. F is a function of no arguments
which determines the colour seen along RAY."
  (let ((hs (sort (remove not (map (lambda (o) (o ray world)) world))
                  (lambda (a b) (< (car a) (car b))))))
    (if (or (null? hs)
            (> (ray-bounces ray) 5))
        (bg ray)
        ((cdr (car hs))))))

(define (write-ppm s w h f)
  (display "P3" s) (newline s)
  (format s "~d ~d" w h) (newline s)
  (display "255" s) (newline s)
  (let ((aspect (/ w h)))
    (do ((y 0 (1+ y))) ((= y h))
      (do ((x 0 (1+ x))) ((= x w))
        (format #t "~d ~d\n" x y)
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
                                 aspect (* -1 aspect)
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
      (newline s))))

(define (img-fun x y)
  (trace world (make-ray
                (make-vec3 0 0 0)
                (make-vec3 x y -1)
                0)))


(call-with-output-file "img.ppm"
  (lambda (s)
    (write-ppm s  100 80 img-fun)))

;;
;;
;;
