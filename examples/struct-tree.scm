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

(define (fold-struct-tree obj)
  (let ((type (ffi-cos-type-of obj))
        (ref (ffi-ref-id obj)))
    (let ((val (case type

                 ((<ffi-cos-array>)
                      (list->vector (ffi-arr-foldr
                                      (lambda (val va)
                                        (cons (fold-struct-tree val) va))
                                      '()
                                      obj)))

                 ((<ffi-cos-stream> <ffi-cos-dictionary>)
                      (cons (if (eq? type '<ffi-cos-stream>) '@stream '@dict)
                            (ffi-dic-foldr
                              (lambda (dic-key dic-val va)
                                (if dic-key
                                    (let ((val (if (memq dic-key '(/P /Pg /ParentTree /RoleMap))
                                                   (ffi-ref-id dic-val)
                                                   (fold-struct-tree dic-val))))
                                      (cons (cons dic-key val) va))
                                    (cons (cons (list "invalid key of " (ffi-ref-id obj)) dic-val) va)))
                              '()
                              obj)))

                 ((<ffi-cos-name>)
                      (ffi-obj-name obj))

                 ((<ffi-cos-bool>)      (ffi-obj-bool obj))
                 ((<ffi-cos-number>)    (ffi-obj-number obj))
                 ((<ffi-cos-real>)      (ffi-obj-real obj))
                 ((<ffi-cos-string>)    (ffi-obj-string obj))
                 ((<ffi-cos-hexstring>) (ffi-obj-hexstring obj))
                 ((<ffi-cos-null>)      'null)
                 ((<ffi-cos-rawdata>)   type)
                 (else                  obj))))
      (if ref (list '@ref ref val) val))))

;;(define (get-struct-tree doc)
;;  (fold-struct-tree (ffi-doc-struct-tree-root doc)))

(define (get-struct-tree doc)
  (let* ((cat (ffi-doc-catalog doc))
         (root (ffi-dic-indirect cat "StructTreeRoot")))
    (fold-struct-tree root)))

(define (open-document file-name)
  (let ((doc (ffi-create-mem-doc)))
    (if (ffi-doc-load doc file-name) doc
        (begin (ffi-dispose doc) #f))))

(define (main . args)
  (let* ((pdf-name (if (null? args) "hello.pdf" (car args)))
         (doc (open-document pdf-name)))
    (if (not (ffi-mem-doc? doc))
        (println "unable to open pdf file: " pdf-name)
        (let ((st (get-struct-tree doc)))
          (ffi-dispose doc)
          (if (not st)
              (println "PDF file '" pdf-name "' doesn't have defined struct tree!")
              (pprint st))))))
