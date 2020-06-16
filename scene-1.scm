(use-modules
 (ray)
 (vector))

(define (tr ray)
  (trace (list (sphere (make-vec3 0.0 1.0 -1.0) 0.7 (perfectly-reflective tr))
               (sphere (make-vec3 -1.3 1.1 -1.0) 0.4 (diffuse-colour tr 1 0 0 0.8))
               (sphere (make-vec3 -0.7 0.7 0.0) 0.4 totally-black)
               (sphere (make-vec3 1.3 1.1 -1.0) 0.4 (diffuse-colour tr 0 0 1 0.8))
               (sphere (make-vec3 0.7 0.7 0.0) 0.4 totally-random)
               (inf-plane 0.0))
         ray))

(render "scene-1" tr 800 600 0.05)
