(use-modules
 (ray)
 (vector)
 (statprof))

(define (tr ray)
  (trace (list (sphere (v3 0.0 1.0 -1.0) 0.7 (perfectly-reflective tr))
               (sphere (v3 -1.3 1.1 -1.0) 0.4 (diffuse-colour tr 1 0 0 0.8))
               (sphere (v3 -0.7 0.7 0.0) 0.4 totally-black)
               (sphere (v3 1.3 1.1 -1.0) 0.4 (diffuse-colour tr 0 0 1 0.8))
               (sphere (v3 0.7 0.7 0.0) 0.4 totally-random)
               (inf-plane 0.0))
         ray))

(statprof
 (lambda () (render "scene-1" tr 800 600 0.05)))
