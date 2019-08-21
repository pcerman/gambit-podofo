;; Copyright (c) 2019 Peter Cerman (https://github.com/pcerman)
;;
;; gambit-podofo: This is gambit scheme (http://gambitscheme.org)
;; binding to the podofo library (http://podofo.sourceforge.net).
;;
;; Original source code if this library is published on github
;; resository (https://github.com/pcerman/gambit-podofo).
;;
;; It is released under GNU General Public License v3.0

(define-macro (add1 n)
  `(+ ,n 1))

(define-macro (sub1 n)
  `(- ,n 1))

(define (pprint obj)
  (define (pprefix lvl)
    (newline)
    (let loop ((lvl lvl))
      (when (positive? lvl)
            (display "  ")
            (loop (sub1 lvl)))))

  (define (vector-inline? vec)
    (and (vector? vec)
         (< (vector-length vec) 5)
         (let loop ((i 0))
           (if (>= i (vector-length vec)) #t
               (let ((elm (vector-ref vec i)))
                 (cond ((or (pair? elm)
                            (vector? elm)) #f)
                       ((string? elm)
                            (and (< (string-length elm) 7)
                                 (loop (add1 i))))
                       (else (loop (add1 i)))))))))

  (define (pprint-inline? obj)
    (cond ((pair? obj)
               (and (not (pair? (cdr obj)))
                    (or (vector-inline? (cdr obj))
                        (not (vector? (cdr obj))))))
          ((vector? obj)
               (vector-inline? obj))
          (else #t)))

  (define (%pp lvl obj nl?)
    (when nl?
          (pprefix lvl))

    (cond ((pprint-inline? obj)
               (write obj))
          ((pair? obj)
               (display "(")
               (%pp (add1 lvl) (car obj) #f)
               (let loop ((obj (cdr obj)) (idx 0))
                 (cond ((pair? obj)
                            (if (and (< idx 2) (symbol? (car obj)))
                                (begin
                                  (display " ")
                                  (%pp (add1 lvl) (car obj) #f))
                                (%pp (add1 lvl) (car obj) #t))
                            (loop (cdr obj) (add1 idx)))
                       ((null? obj)
                            (display ")"))
                       (else
                            (pprefix (add1 lvl))
                            (display ".")
                            (%pp (add1 lvl) obj #t)
                            (display ")")))))
          ((vector? obj)
               (display "#(")
               (let loop ((i 0))
                 (when (< i (vector-length obj))
                       (%pp (add1 lvl) (vector-ref obj i) (positive? i))
                       (loop (add1 i))))
               (pprefix lvl)
               (display ")"))
          (else
               (write obj))))
  (%pp 0 obj #f)
  (newline))
