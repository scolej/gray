(define-module (vector)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs base)
  #:export (v3
            v3x v3y v3z
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

(define-syntax vmap1
  (syntax-rules ()
    ;; Map over vector components using F,
    ;; then join the results using G.
    ((_ g f a)
     (g (f (v3x a))
        (f (v3y a))
        (f (v3z a))))
    ((_ g f a b)
     (g (f (v3x a) (v3x b))
        (f (v3y a) (v3y b))
        (f (v3z a) (v3z b))))))

;; Map over vector components and then build a new vector.
(define-syntax vmap
  (syntax-rules ()
    ((_ f a) (vmap1 v3 f a))
    ((_ f a b) (vmap1 v3 f a b))))

(define vzero (v3 0 0 0))

(define (assert-v3 v) (assert (v3? v)))

(define (v+ a b) (vmap + a b))
(define (v- a b) (vmap - a b))

(define (v* v s)
  (let ((f (lambda (x) (* s x))))
    (vmap f v)))

(define (vdot a b) (vmap1 + * a b))

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
