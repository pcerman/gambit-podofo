
;; Copyright (c) 2019 Peter Cerman (https://github.com/pcerman)
;;
;; gambit-podofo: This is gambit scheme (http://gambitscheme.org)
;; binding to the podofo library (http://podofo.sourceforge.net).
;;
;; Original source code if this library is published on github
;; resository (https://github.com/pcerman/gambit-podofo).
;;
;; It is released under GNU General Public License v3.0

(##namespace ("PoDoFo#"))

(##include "~~lib/gambit#.scm")
(##include "ffi-podofo#.scm")

(declare
  (standard-bindings)
  (block)
  (fixnum))

(c-declare #<<c-declare-end

#include <stdlib.h>
#include "ffi-pdf.h"
#include "ffi-release.h"

c-declare-end
)

(c-initialize "ffi_init();")

(c-define-type u8string nonnull-UTF-8-string)

(c-define-type PDF-MDOC  "PDF_MDOC")
(c-define-type PDF-MDOC* (pointer PDF-MDOC (PDF-MDOC*) "ffi_ptr_release"))

(c-define-type PDF-COBJ  "PDF_COBJ")
(c-define-type PDF-COBJ* (pointer PDF-COBJ (PDF-COBJ*) "ffi_ptr_release"))

(c-define-type PDF-STREAM  "PDF_STREAM")
(c-define-type PDF-STREAM* (pointer PDF-STREAM (PDF-STREAM*) "ffi_ptr_release"))

(c-define-type PDF-PTR  "PDF_PTR")
(c-define-type PDF-PTR* (pointer PDF-PTR (PDF-PTR* PDF-MDOC* PDF-COBJ* PDF-STREAM*)
                                 "ffi_ptr_release"))

;; ---------------------------------------------------------------------

(define-macro (get-string obj)
  `(if (symbol? ,obj)
       (symbol->string ,obj)
       ,obj))

(define-macro (get-symbol obj)
  `(if (string? ,obj)
       (string->symbol ,obj)
       ,obj))

(define-macro (get-symbol? to-symbol obj)
  `(if ,to-symbol
       (get-symbol ,obj)
       ,obj))

(define (find p? lst)
  (cond ((pair? lst) (if (p? (car lst)) lst
                         (find p? (cdr lst))))
        ((null? lst) #f)
        (else        (p? lst))))

;; ----- ffi -----------------------------------------------------------

(define podofo-version
  (c-lambda ()
            char-string
    "podofo_version"))

(define ffi-version
  (c-lambda ()
            char-string
    "ffi_version"))

(define (ffi-podofo? obj)
  (define podofo-tags '(PDF-COBJ* PDF-STREAM* PDF-MDOC* PDF-PTR*))

  (and (foreign? obj)
       (find (lambda (tag)
               (memq tag podofo-tags))
             (foreign-tags obj))
       (ffi-type-of obj)))

(define ffi-dispose
  (c-lambda (PDF-PTR*)
            void
    "ffi_dispose"))

(define (ffi-type-of obj)
  (define %type-of
    (c-lambda (PDF-PTR*)
              char-string
      "ffi_type_of"))
  (let ((type (and (foreign? obj) (%type-of obj))))
    (get-symbol type)))

(define (ffi-cos-type-of obj)
  (define %type-of
    (c-lambda (PDF-PTR*)
              char-string
      "ffi_cos_type_of"))
  (let ((type (and (foreign? obj) (%type-of obj))))
    (get-symbol type)))

;; ----- Document ------------------------------------------------------

(define ffi-create-mem-doc
  (c-lambda ()
            PDF-MDOC*
    "ffi_create_mem_doc"))

(define ffi-doc-version
  (c-lambda (PDF-MDOC*)
            char-string
    "ffi_doc_version"))

(define ffi-doc-set-version
  (c-lambda (PDF-MDOC* char-string)
            bool
    "ffi_doc_set_version"))

(define ffi-doc-load
  (c-lambda (PDF-MDOC* u8string)
            bool
    "ffi_doc_load"))

(define ffi-doc-write
  (c-lambda (PDF-MDOC* u8string)
            bool
    "ffi_doc_write"))

(define ffi-doc-catalog
  (c-lambda (PDF-MDOC*)
            PDF-COBJ*
    "ffi_doc_catalog"))

(define ffi-doc-trailer
  (c-lambda (PDF-MDOC*)
            PDF-COBJ*
    "ffi_doc_trailer"))

(define ffi-doc-struct-tree-root
  (c-lambda (PDF-MDOC*)
            PDF-COBJ*
    "ffi_doc_struct_tree_root"))

(define (ffi-mem-doc? obj)
  (and (foreign? obj)
       (memq 'PDF-MDOC* (foreign-tags obj))
       (eq? (ffi-type-of obj) '<ffi-mem-doc>)))

;; ----- Object --------------------------------------------------------

(define-macro (define-predicate predicate native)
  `(define (,predicate obj)
     (define %obj-type?
       (c-lambda (PDF-COBJ*)
                 bool
         ,native))
     (and (foreign? obj)
          (memq 'PDF-COBJ* (foreign-tags obj))
          (%obj-type? obj))))

(define (ffi-cos? obj)
  (and (foreign? obj)
       (memq 'PDF-COBJ* (foreign-tags obj))
       (eq? (ffi-type-of obj) '<ffi-cos>)))

(define-predicate ffi-obj-bool?       "ffi_obj_is_bool")
(define-predicate ffi-obj-number?     "ffi_obj_is_number")
(define-predicate ffi-obj-real?       "ffi_obj_is_real")
(define-predicate ffi-obj-string?     "ffi_obj_is_string")
(define-predicate ffi-obj-hexstring?  "ffi_obj_is_hexstring")
(define-predicate ffi-obj-name?       "ffi_obj_is_name")
(define-predicate ffi-obj-array?      "ffi_obj_is_array")
(define-predicate ffi-obj-dictionary? "ffi_obj_is_dictionary")
(define-predicate ffi-obj-rawdata?    "ffi_obj_is_rawdata")
(define-predicate ffi-obj-null?       "ffi_obj_is_null")
(define-predicate ffi-obj-reference?  "ffi_obj_is_reference")
(define-predicate ffi-obj-stream?     "ffi_obj_has_stream")

(define ffi-obj-bool
  (c-lambda (PDF-COBJ*)
            bool
    "ffi_obj_bool"))

(define ffi-obj-number
  (c-lambda (PDF-COBJ*)
            int64
    "ffi_obj_number"))

(define ffi-obj-real
  (c-lambda (PDF-COBJ*)
            float64
    "ffi_obj_real"))

(define ffi-obj-string
  (c-lambda (PDF-COBJ*)
            u8string
#<<c-lambda-end
   char * s = ffi_obj_string(___arg1);
   ___return(s);
   #define ___AT_END if (s != NULL) free (s);
c-lambda-end
))

(define ffi-obj-hexstring
  (c-lambda (PDF-COBJ*)
            u8string
#<<c-lambda-end
   char * s = ffi_obj_hexstring(___arg1);
   ___return(s);
   #define ___AT_END if (s != NULL) free (s);
c-lambda-end
))

(define (ffi-obj-name obj #!key (as-symbol #t))
  (define %obj-name
    (c-lambda (PDF-COBJ*)
              char-string
      "ffi_obj_name"))
  (let ((name (%obj-name obj)))
    (get-symbol? as-symbol name)))

;; ----- Reference -----------------------------------------------------

(define (ffi-ref-id obj #!key (as-symbol #t))
  (define %ref-id
    (c-lambda (PDF-COBJ*)
              char-string
      "ffi_ref_id"))
  (let ((id (%ref-id obj)))
    (get-symbol? as-symbol id)))

(define ffi-ref-indirect
  (c-lambda (PDF-COBJ*)
            PDF-COBJ*
    "ffi_ref_indirect"))

;; ----- Array ---------------------------------------------------------

(define ffi-arr-size
  (c-lambda (PDF-COBJ*)
            int
    "ffi_arr_size"))

(define ffi-arr-value
  (c-lambda (PDF-COBJ* int)
            PDF-COBJ*
    "ffi_arr_value"))

(define ffi-arr-indirect
  (c-lambda (PDF-COBJ* int)
            PDF-COBJ*
    "ffi_arr_indirect"))

(define (ffi-arr-values arr #!key (indirect #t))
  (let loop ((i (- (ffi-arr-size arr) 1)) (lst '()))
    (if (< i 0) lst
        (loop (- i 1)
              (cons (if indirect
                        (ffi-arr-indirect arr i)
                        (ffi-arr-value arr i))
                    lst)))))

(define (ffi-arr-foldl fn va arr #!key (indirect #t))
  (let ((size (ffi-arr-size arr)))
    (let loop ((i 0) (va va))
      (if (>= i size) va
          (loop (+ i 1)
                (fn (if indirect
                        (ffi-arr-indirect arr i)
                        (ffi-arr-value arr i))
                    va))))))

(define (ffi-arr-foldr fn va arr #!key (indirect #t))
  (let loop ((i (- (ffi-arr-size arr) 1)) (va va))
      (if (< i 0) va
          (loop (- i 1)
                (fn (if indirect
                        (ffi-arr-indirect arr i)
                        (ffi-arr-value arr i))
                    va)))))

(define ffi-arr-resize
  (c-lambda (PDF-COBJ* int)
            bool
    "ffi_arr_resize"))

(define ffi-arr-remove
  (c-lambda (PDF-COBJ* int)
            bool
    "ffi_arr_remove"))

(define ffi-arr-insert
  (c-lambda (PDF-COBJ* int)
            bool
    "ffi_arr_insert"))

(define-macro (define-arr-setter setter type cfun #!key (as-string #f))
  `(define (,setter arr idx value #!key (indirect #f))
     (define %arr-set
       (c-lambda (PDF-COBJ* int ,type bool)
                 PDF-COBJ*
         ,cfun))
     ,(if as-string
          '(%arr-set arr idx (get-string value) indirect)
          '(%arr-set arr idx value indirect))))

(define-arr-setter ffi-arr-set-bool       bool          "ffi_arr_set_bool")
(define-arr-setter ffi-arr-set-number     int64         "ffi_arr_set_number")
(define-arr-setter ffi-arr-set-real       double        "ffi_arr_set_real")
(define-arr-setter ffi-arr-set-string     u8string      "ffi_arr_set_string")
(define-arr-setter ffi-arr-set-hexstring  char-string   "ffi_arr_set_hexstring")
(define-arr-setter ffi-arr-set-name       char-string   "ffi_arr_set_name" as-string: #t)

(define ffi-arr-set-null
   (c-lambda (PDF-COBJ* int)
             PDF-COBJ*
     "ffi_arr_set_null"))

(define (ffi-arr-set-dictionary arr idx #!key (indirect #t))
   (define %arr-set
     (c-lambda (PDF-COBJ* int bool)
               PDF-COBJ*
       "ffi_arr_set_dictionary"))
   (%arr-set arr idx indirect))

(define (ffi-arr-set-array arr idx #!key (indirect #t))
   (define %arr-set
     (c-lambda (PDF-COBJ* int bool)
               PDF-COBJ*
       "ffi_arr_set_array"))
   (%arr-set arr idx indirect))

(define ffi-arr-set-reference
   (c-lambda (PDF-COBJ* int PDF-COBJ*)
             bool
     "ffi_arr_set_reference"))

;; ----- Dictionary ----------------------------------------------------

(define ffi-dic-size
  (c-lambda (PDF-COBJ*)
            int
    "ffi_dic_size"))

(define (ffi-dic-key dic idx #!key (as-symbol #t))
  (define %dic-key
    (c-lambda (PDF-COBJ* int)
              char-string
      "ffi_dic_key"))
  (let ((key (%dic-key dic idx)))
    (get-symbol? as-symbol key)))

(define (ffi-dic-value dic key)
  (define %dic-value
    (c-lambda (PDF-COBJ* char-string)
              PDF-COBJ*
      "ffi_dic_value"))
  (%dic-value dic (get-string key)))

(define (ffi-dic-indirect dic key)
  (define %dic-indirect
    (c-lambda (PDF-COBJ* char-string)
              PDF-COBJ*
      "ffi_dic_indirect"))
  (%dic-indirect dic (get-string key)))

(define (ffi-dic-keys dic #!key (as-symbol #t))
  (let loop ((i (- (ffi-dic-size dic) 1)) (keys '()))
    (if (< i 0) keys
        (loop (- i 1)
              (cons (ffi-dic-key dic i as-symbol: as-symbol)
                    keys)))))

(define (ffi-dic-values dic #!key (indirect #t) (as-symbol #t))
  (map (lambda (key)
         (cons (get-symbol? as-symbol key)
               (if indirect
                   (ffi-dic-indirect dic key)
                   (ffi-dic-value dic key))))
       (ffi-dic-keys dic as-symbol: #f)))

(define (ffi-dic-foldl fn va dic #!key (indirect #t) (as-symbol #t))
  (let ((size (ffi-dic-size dic)))
    (let loop ((i 0) (va va))
      (if (>= i size) va
          (let* ((key (ffi-dic-key dic i as-symbol: #f))
                 (val (if indirect
                          (ffi-dic-indirect dic key)
                          (ffi-dic-value dic key))))
            (loop (+ i 1) (fn (get-symbol? as-symbol key) val va)))))))

(define (ffi-dic-foldr fn va dic #!key (indirect #t) (as-symbol #t))
  (let loop ((i (- (ffi-dic-size dic) 1)) (va va))
    (if (< i 0) va
        (let* ((key (ffi-dic-key dic i as-symbol: #f))
               (val (if indirect
                        (ffi-dic-indirect dic key)
                        (ffi-dic-value dic key))))
          (loop (- i 1) (fn (get-symbol? as-symbol key) val va))))))

(define (ffi-dic-remove dic key)
  (define %dic-remove
    (c-lambda (PDF-COBJ* char-string)
              bool
      "ffi_dic_remove"))
  (%dic-remove dic (get-string key)))

(define-macro (define-dic-setter setter type cfun #!key (as-string #f))
  `(define (,setter dic key value #!key (indirect #f))
     (define %dic-set
       (c-lambda (PDF-COBJ* char-string ,type bool)
                 PDF-COBJ*
         ,cfun))
     ,(if as-string
          '(%dic-set dic (get-string key) (get-string value) indirect)
          '(%dic-set dic (get-string key) value indirect))))

(define-dic-setter ffi-dic-set-bool      bool        "ffi_dic_set_bool")
(define-dic-setter ffi-dic-set-number    int64       "ffi_dic_set_number")
(define-dic-setter ffi-dic-set-real      double      "ffi_dic_set_real")
(define-dic-setter ffi-dic-set-string    u8string    "ffi_dic_set_string")
(define-dic-setter ffi-dic-set-hexstring char-string "ffi_dic_set_hexstring")
(define-dic-setter ffi-dic-set-name      char-string "ffi_dic_set_name" as-string: #t)

(define (ffi-dic-set-null dic key)
   (define %dic-set
     (c-lambda (PDF-COBJ* char-string)
               PDF-COBJ*
       "ffi_dic_set_null"))
   (%dic-set dic (get-string key)))

(define (ffi-dic-set-dictionary dic key #!key (indirect #t))
   (define %dic-set
     (c-lambda (PDF-COBJ* char-string bool)
               PDF-COBJ*
       "ffi_dic_set_dictionary"))
   (%dic-set dic (get-string key) indirect))

(define (ffi-dic-set-array dic key #!key (indirect #t))
   (define %dic-set
     (c-lambda (PDF-COBJ* char-string bool)
               PDF-COBJ*
       "ffi_dic_set_array"))
   (%dic-set dic (get-string key) indirect))

(define ffi-dic-set-reference
   (c-lambda (PDF-COBJ* char-string PDF-COBJ*)
             bool
     "ffi_dic_set_reference"))

(define ffi-dic-stream
  (c-lambda (PDF-COBJ*)
            PDF-STREAM*
    "ffi_dic_stream"))

(define ffi-dic-filtered-stream
  (c-lambda (PDF-COBJ*)
            PDF-STREAM*
    "ffi_dic_filtered_stream"))

(define ffi-dic-set-stream
  (c-lambda (PDF-COBJ* PDF-STREAM*)
            bool
    "ffi_dic_set_stream"))

;; ----- Stream --------------------------------------------------------

(define ffi-create-stream
  (c-lambda (int)
            PDF-STREAM*
    "ffi_create_stream"))

(define ffi-stream-capacity
  (c-lambda (PDF-STREAM*)
            int
    "ffi_stream_capacity"))

(define ffi-stream-set-capacity
  (c-lambda (PDF-STREAM* int)
            int
    "ffi_stream_set_capacity"))

(define ffi-stream-size
  (c-lambda (PDF-STREAM*)
            int
    "ffi_stream_size"))

(define ffi-stream-set-size
  (c-lambda (PDF-STREAM* int)
            int
    "ffi_stream_set_size"))

(define ffi-stream-position
  (c-lambda (PDF-STREAM*)
            int
    "ffi_stream_position"))

(define ffi-stream-set-position
  (c-lambda (PDF-STREAM* int)
            int
    "ffi_stream_set_position"))

(define ffi-stream-available
  (c-lambda (PDF-STREAM*)
            int
    "ffi_stream_available"))

(define ffi-stream-eof?
  (c-lambda (PDF-STREAM*)
            bool
    "ffi_stream_is_eof"))

(define ffi-stream-read
  (c-lambda (PDF-STREAM* scheme-object)
            int
#<<c-lambda-end

if (___U8VECTORP(___arg2) && ___MUTABLEP(___arg2))
{
  unsigned char * ptr = ___CAST(___U8*, ___BODY_AS(___arg2, ___tSUBTYPED));
  int size = ___INT(___U8VECTORLENGTH(___arg2));

  ___result = ffi_stream_read(___arg1, ptr, size);
}
else
{
  ___result = 0;
}

c-lambda-end
))

(define ffi-stream-write
  (c-lambda (PDF-STREAM* scheme-object)
            int
#<<c-lambda-end

if (___U8VECTORP(___arg2))
{
  unsigned char * ptr = ___CAST(___U8*, ___BODY_AS(___arg2, ___tSUBTYPED));
  int size = ___INT(___U8VECTORLENGTH(___arg2));

  ___result = ffi_stream_write(___arg1, ptr, size);
}
else
{
  ___result = 0;
}

c-lambda-end
))

(define ffi-stream-print
  (c-lambda (PDF-STREAM* u8string)
            int
    "ffi_stream_print"))

(define ffi-stream-dispose
  (c-lambda (PDF-STREAM*)
            bool
    "ffi_stream_dispose"))

;; ----- Utils ---------------------------------------------------------

(define ffi-release-count
  (c-lambda ()
            int
    "___result = gReleaseCount;"))
