(compile "vector.scm")

(use-modules
 (ice-9 format)
 ;; (srfi srfi-9)
 )

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

(define (trace-ray world ray)
  (let ((v (vec3-y (ray-dir ray))))
    (list (rescale -1 1 0.5 0.9 v)
          (rescale -1 1 0.5 0.9 v)
          (rescale -1 1 0.9 1.0 v))))

(define (write-ppm s w h f)
  (display "P3" s) (newline s)
  (format s "~d ~d" w h) (newline s)
  (display "255" s) (newline s)
  (do ((y 0 (1+ y))) ((= y h))
    (do ((x 0 (1+ x))) ((= x w))
      (display
       (string-join
        (map (lambda (c)
               (format #f "~d" (inexact->exact (round (lerp 0 255 c)))))
             (f (rescale 0 w 1 -1 x)
                (rescale 0 h 1 -1 y))) " ")
       s)
      (display " " s))
    (newline s)))

(define (img-fun x y)
  ;; (format #t "~,2f ~,2f\n" x y)
  (trace-ray #f (make-ray (make-vec3 0 0 0)
                          (make-vec3 x y -1))))


(call-with-output-file "img.ppm"
  (lambda (s)
      (write-ppm s 300 200 img-fun)))

;;
;;
;;
