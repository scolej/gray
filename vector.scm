(define-module (vector)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs base)
  #:use-module (rnrs arithmetic flonums)
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
  (make-v3 x y z)
  v3?
  (x v3x)
  (y v3y)
  (z v3z))

(define (v3 x y z)
  (make-v3
   (real->flonum x)
   (real->flonum y)
   (real->flonum z)))

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

(define (v+ a b) (vmap fl+ a b))
(define (v- a b) (vmap fl- a b))

(define (v* v s)
  (let* ((fls (real->flonum s))
         (f (lambda (x) (fl* fls x))))
    (vmap f v)))

(define (vdot a b) (vmap1 fl+ fl* a b))

(define (vcross a b)
  (v3 (fl- (fl* (v3y a) (v3z b)) (fl* (v3z a) (v3y b)))
      (fl- (fl* (v3z a) (v3x b)) (fl* (v3x a) (v3z b)))
      (fl- (fl* (v3x a) (v3y b)) (fl* (v3y a) (v3x b)))))

(define (vnorm v)
  (let ((x (v3x v))
        (y (v3y v))
        (z (v3z v)))
    (flsqrt (fl+ (fl* x x)
                 (fl* y y)
                 (fl* z z)))))

(define (vunit v)
  (let ((n (vnorm v)))
    (if (= 0 n) vzero
        (v* v (fl/ 1.0 n)))))
