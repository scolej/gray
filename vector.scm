(define-module (vector)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs base)
  #:export (v3
            v3x v3y v3z
            v3->list
            assert-v3
            v+
            v-
            v*
            vdot
            vcross
            vnorm
            vunit))

(define-record-type vec3
  (v3 x y z)
  v3?
  (x v3x)
  (y v3y)
  (z v3z))

(define vzero (v3 0 0 0))

(define (v3->list v)
  (list (v3x v) (v3y v) (v3z v)))

(define (vmap f . vs)
  (v3 (apply f (map v3x vs))
      (apply f (map v3y vs))
      (apply f (map v3z vs))))

(define (assert-v3 v) (assert (v3? v)))

(define (v+ . vs) (apply vmap + vs))
(define (v- . vs) (apply vmap - vs))
(define (v* v s) (vmap (lambda (x) (* s x)) v))
(define (vdot a b) (apply + (v3->list (vmap * a b))))

(define (vcross a b)
  (v3 (- (* (v3y a) (v3z b)) (* (v3z a) (v3y b)))
      (- (* (v3z a) (v3x b)) (* (v3x a) (v3z b)))
      (- (* (v3x a) (v3y b)) (* (v3y a) (v3x b)))))

(define (vnorm v)
  (let ((x (v3x v))
        (y (v3y v))
        (z (v3z v)))
    (sqrt (+ (* x x)
             (* y y)
             (* z z)))))

(define (vunit v)
  (let ((n (vnorm v)))
    (if (= 0 n) vzero
        (v* v (/ 1.0 n)))))
