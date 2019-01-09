(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (null? x) (eq? x '()))

(define (not x)
  (if x false true))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (and a b)
  (if a (if b true false) false))

(define (nand a b)
  (not (and a b)))

(define (or a b)
  (if a a (if b b false)))

(define (nor a b)
  (not (or a b)))

(define (xor a b)
  (if a (if b false true) (if b true false)))

(define (abs x)
  (if (< x 0) (* -1 x) x))

(define (foldl proc init list)
  (if (null? list)
      init
      (foldl proc
             (proc init (car list))
             (cdr list))))

(define (foldr proc init list)
  (if (null? list)
      init
      (proc (car list)
            (foldr proc init (cdr list)))))

(define (list . items)
  (foldr cons '() items))

(define (reverse list)
  (foldl (lambda (a x) (cons x a)) '() list))

(define (unary-map proc list)
  (foldr (lambda (x rest) (cons (proc x) rest))
         '()
         list))

(define (map proc . arg-lists)
  (if (car arg-lists)
      (cons (apply proc (unary-map car arg-lists))
            (apply map (cons proc
                             (unary-map cdr arg-lists))))
      '()))

(define (append a b) (foldr cons b a))
