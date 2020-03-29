(define (write-ppm s w h f)
  (display "P3" s) (newline s)
  (format s "~d ~d" w h) (newline s)
  (display "255" s) (newline s)
  (do ((x 0 (1+ x))) ((= x w))
    (do ((y 0 (1+ y))) ((= y h))
      (display (string-join (map number->string (f x y)) " ") s)
      (display " " s))
    (newline s)))

(define (img-fun x y)
  (list (random 255)
        (random 255)
        (random 255)))

(call-with-output-file "img.ppm"
  (lambda (s)
      (write-ppm s 300 200 img-fun)))
