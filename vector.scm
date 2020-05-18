(define-module (vector)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs base)
  #:export (make-vec3
            vec3-x vec3-y vec3-z
            v->list
            assert-vec3
            v+
            v-
            v*
            vdot
            vcross
            vnorm
            vunit))

(define-record-type vec3
  (make-vec3 x y z)
  vec3?
  (x vec3-x)
  (y vec3-y)
  (z vec3-z))

(define vzero (make-vec3 0 0 0))

(define (v->list v)
  (list (vec3-x v) (vec3-y v) (vec3-z v)))

(define (vmap f . vs)
  (make-vec3 (apply f (map vec3-x vs))
             (apply f (map vec3-y vs))
             (apply f (map vec3-z vs))))

(define (assert-vec3 v) (assert (vec3? v)))

(define (v+ . vs) (apply vmap + vs))
(define (v- . vs) (apply vmap - vs))
(define (v* v s) (vmap (lambda (x) (* s x)) v))
(define (vdot a b) (apply + (v->list (vmap * a b))))

(define (vcross a b)
  (make-vec3 (- (* (vec3-y a) (vec3-z b)) (* (vec3-z a) (vec3-y b)))
             (- (* (vec3-z a) (vec3-x b)) (* (vec3-x a) (vec3-z b)))
             (- (* (vec3-x a) (vec3-y b)) (* (vec3-y a) (vec3-x b)))))

(define (vnorm v)
  (let ((x (vec3-x v))
        (y (vec3-y v))
        (z (vec3-z v)))
    (sqrt (+ (* x x)
             (* y y)
             (* z z)))))

(define (vunit v)
  (let ((n (vnorm v)))
    (if (= 0 n) vzero
        (v* v (/ 1.0 n)))))
