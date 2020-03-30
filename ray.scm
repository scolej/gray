(use-modules
 (srfi srfi-9)
 (ice-9 format)
 (vector))

(define-record-type ray
  (make-ray origin direction)
  ray?
  (origin ray-origin)
  (direction ray-dir))

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

(define (trace-ray-bg ray)
  (let ((v (vec3-y (ray-dir ray))))
    (list (rescale -1 1 0.5 0.9 v)
          (rescale -1 1 0.5 0.9 v)
          (rescale -1 1 0.9 1.0 v))))

(define (trace-ray-sphere pos rad world)
  (lambda (ray)
    (let* ((oc (v- (ray-origin ray) pos))
           (dir (ray-dir ray))
           (a (vdot dir dir))
           (b (* 2.0 (vdot oc dir)))
           (c (- (vdot oc oc) (* rad rad)))
           (descr (- (* b b) (* 4 a c))))
      (if (> descr 0) (list 1 0 0) (world ray)))))

(define trace-ray
  (trace-ray-sphere (make-vec3 0 0 -3.0) 1.0 trace-ray-bg))

(define (write-ppm s w h f)
  (display "P3" s) (newline s)
  (format s "~d ~d" w h) (newline s)
  (display "255" s) (newline s)
  (let ((aspect (/ w h)))
    (do ((y 0 (1+ y))) ((= y h))
      (do ((x 0 (1+ x))) ((= x w))
        (display
         (string-join
          (map (lambda (c)
                 (format #f "~d" (inexact->exact (round (lerp 0 255 c)))))
               (f (rescale 0 w aspect (* -1 aspect) x)
                  (rescale 0 h 1 -1 y))) " ")
         s)
        (display " " s))
      (newline s))))

(define (img-fun x y)
  ;; (format #t "~,2f ~,2f\n" x y)
  (trace-ray (make-ray (make-vec3 0 0 0)
                       (make-vec3 x y -1))))


(call-with-output-file "img.ppm"
  (lambda (s)
      (write-ppm s 300 200 img-fun)))

;;
;;
;;
