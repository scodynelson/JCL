(lambda () 50)

(eval-when () #\b)

(flet ((foo (x) x))
  (foo 1))

100

(set-symbol-function 'bar (lambda () 20))