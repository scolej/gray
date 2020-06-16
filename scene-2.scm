(use-modules (ray) (vector))

(define (tr r)
  (trace world r))

(define world
  (list
   (sphere (make-vec3 0 1.1 -1.0) 0.4 (diffuse-colour tr 1 0 0 0.8))
   ))

(render "scene-2" world 800 600 0.1)
