#!/usr/bin/env gsi-script

;; Copyright (c) 2019 Peter Cerman (https://github.com/pcerman)
;;
;; gambit-podofo: This is gambit scheme (http://gambitscheme.org)
;; binding to the podofo library (http://podofo.sourceforge.net).
;;
;; Original source code if this library is published on github
;; resository (https://github.com/pcerman/gambit-podofo).
;;
;; It is released under GNU General Public License v3.0

(include "~~lib/pdf/podofo.scm")

(define (create-page doc width height)
  (let* ((cat (ffi-doc-catalog doc))
         (pgs (ffi-dic-indirect cat "Pages"))
         (kid (ffi-dic-indirect pgs "Kids"))
         (cnt (ffi-dic-indirect pgs "Count"))
         (pg  (ffi-arr-set-dictionary kid -1)))
    (ffi-dic-set-number pgs "Count" (+ (ffi-obj-number cnt) 1))
    (ffi-dic-set-name pg "Type" "Page")
    (let ((box (ffi-dic-set-array pg "MediaBox" indirect: #f)))
      (ffi-arr-set-number box -1 0)
      (ffi-arr-set-number box -1 0)
      (ffi-arr-set-number box -1 width)
      (ffi-arr-set-number box -1 height))
    (ffi-dic-set-reference pg "Parent" pgs)
    (ffi-dic-set-dictionary pg "Contents")
    pg))

(define (main . args)
  (let* ((pdf-name (if (null? args) "test.pdf" (car args)))
         (doc (ffi-create-mem-doc))
         (pg (create-page doc 100 150)))

    (ffi-doc-set-version doc "1.7")

    (let ((cnt (ffi-dic-indirect pg "Contents"))
          (str (ffi-create-stream 512)))
      (ffi-dic-set-number cnt "Length" 0)
    
      (ffi-stream-print str "10 20 m\n")
      (ffi-stream-print str "50 130 l\n")
      (ffi-stream-print str "90 20 l\n")
      (ffi-stream-print str "S ")
    
      (ffi-dic-set-stream cnt str))

    (ffi-doc-write doc pdf-name)))
