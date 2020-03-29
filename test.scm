(define (assert-equal expected actual)
  (unless (equal? expected actual)
    (error #f
           (string-join (list "Assertion failed"
                              (format #f "  Expected: ~s" expected)
                              (format #f "    Actual: ~s" actual))
                        "\n" 'suffix))))