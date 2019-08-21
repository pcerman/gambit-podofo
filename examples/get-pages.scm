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
(include "pprint.scm")

(define (fold-pages fn va obj)
  (let ((type (ffi-cos-type-of obj)))
    (case type
      ((<ffi-cos-dictionary>)
           (let ((obj-type (ffi-obj-name (ffi-dic-indirect obj '/Type))))
             (cond ((eq? obj-type '/Page)

                        (fn obj va))

                   ((eq? obj-type '/Pages)

                        (ffi-arr-foldl (lambda (obj va)
                                         (fold-pages fn va obj))
                                       va
                                       (ffi-dic-indirect obj '/Kids)))

                   (else va))))

      (else va))))

(define (get-pages-from-tree doc)
  (let ((pages (ffi-dic-indirect (ffi-doc-catalog doc) '/Pages)))
    (cdr (fold-pages
           (lambda (obj va)
             (let ((cnt (car va))
                   (pgs (cdr va)))
               (cons (add1 cnt)
                     (cons (cons cnt (ffi-ref-id obj)) pgs))))
           (list 1)
           pages))))

(define (open-document file-name)
  (let ((doc (ffi-create-mem-doc)))
    (if (ffi-doc-load doc file-name) doc
        (begin (ffi-dispose doc) #f))))

(define (main . args)
  (let* ((pdf-name (if (null? args) "hello.pdf" (car args)))
         (doc (open-document pdf-name)))
    (if (not (ffi-mem-doc? doc))
        (println "unable to open pdf file: " pdf-name)
        (let ((ps (get-pages-from-tree doc)))
          (ffi-dispose doc)
          (pprint ps)))))
