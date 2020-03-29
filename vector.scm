(use-modules
 (srfi srfi-9)
 (rnrs base))

(define-record-type vec3
  (make-vec3 x y z)
  vec3?
  (x vec3-x)
  (y vec3-y)
  (z vec3-z))

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

;;
;;
;;

(define (assert-equal expected actual)
  (unless (equal? expected actual)
    (error #f
           (string-join (list "Assertion failed"
                              (format #f "  Expected: ~s" expected)
                              (format #f "    Actual: ~s" actual))
                        "\n" 'suffix))))

(assert-equal
 (make-vec3 1 14 7)
 (v+ (make-vec3 1 2 3)
     (make-vec3 3 3 3)
     (make-vec3 0 0 0)
     (make-vec3 -3 9 1)))

(assert-equal
 (make-vec3 1 -10 -1)
 (v- (make-vec3 1 2 3)
     (make-vec3 3 3 3)
     (make-vec3 0 0 0)
     (make-vec3 -3 9 1)))

(assert-equal
 (make-vec3 4 6 10)
 (v* (make-vec3 2 3 5) 2))

(assert-equal 0 (vdot (make-vec3 1 0 0) (make-vec3 0 1 0)))
(assert-equal 0 (vdot (make-vec3 1 0 0) (make-vec3 0 0 1)))
(assert-equal 20 (vdot (make-vec3 1 2 3) (make-vec3 2 3 4)))

(assert-equal (make-vec3 0 0 0) (vcross (make-vec3 1 0 0) (make-vec3 1 0 0)))
(assert-equal (make-vec3 0 0 1) (vcross (make-vec3 1 0 0) (make-vec3 0 1 0)))
(assert-equal (make-vec3 0 -1 0) (vcross (make-vec3 1 0 0) (make-vec3 0 0 1)))
