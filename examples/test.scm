(include "~~/lib/pdf/podofo.scm")

(define (find-object obj . keys)
  (let loop ((obj obj) (keys keys))
    (cond ((null? keys) obj)
          ((not (eq? (ffi-type-of obj) '<ffi-cos>)) #f)
          (else (loop (ffi-dic-indirect obj (car keys)) (cdr keys))))))

(define (value arg)
  (let ((type (ffi-cos-type-of arg)))
    (if (not type) arg
        (case type
          ((<ffi-cos-bool>)         (ffi-obj-bool arg))
          ((<ffi-cos-number>)       (ffi-obj-number arg))
          ((<ffi-cos-real>)         (ffi-obj-real arg))
          ((<ffi-cos-string>)       (ffi-obj-string arg))
          ((<ffi-cos-hexstring>)    (ffi-obj-hexstring arg))
          ((<ffi-cos-name>)         (ffi-obj-name arg))
          ((<ffi-cos-array>)        type)
          ((<ffi-cos-dictionary>)   type)
          ((<ffi-cos-rawdata>)      type)
          ((<ffi-cos-reference>)    (ffi-ref-id arg))
          ((<ffi-cos-stream>)       type)
          ((<ffi-cos-null>)         type)
          (else                     type)))))

(define (string-pad-left str size)
  (let ((len (string-length str)))
    (if (>= len size) str
        (string-append (make-string (- size len) #\space) str))))

(define (string-pad-right str size)
  (let ((len (string-length str)))
    (if (>= len size) str
        (string-append str (make-string (- size len) #\space)))))

(define (array-values obj)
  (if (not (ffi-obj-array? obj)) (vector)
      (list->vector (map (lambda (i)
                           (ffi-arr-indirect obj i))
                         (iota (ffi-arr-size obj))))))

(define (dict-values obj)
  (if (not (ffi-obj-dictionary? obj)) (list)
      (map (lambda (i)
             (let* ((key (ffi-dic-key obj i))
                    (val (ffi-dic-value obj key)))
               (cons key val)))
           (iota (ffi-dic-size obj)))))

(define (print-value arg)
  (define (print-array type obj)
    (let ((avs (map (lambda (i)
                      (ffi-arr-indirect obj i))
                    (iota (ffi-arr-size obj)))))
      (print "[")
      (for-each (lambda (va)
                  (print " ")
                  (print-value va))
                avs)
      (print " ]")))

  (define (print-dict type obj)
    (let ((avs (map (lambda (i)
                      (let* ((key (ffi-dic-key obj i as-symbol: #f))
                             (val (ffi-dic-value obj key)))
                        (cons key val)))
                    (iota (ffi-dic-size obj)))))
      (println "<<")
      (for-each (lambda (kv)
                  (print "                    " (string-pad-right (car kv) 15) " ")
                  (print-value (cdr kv))
                  (println))
                avs)
      (println "                  >>")))

  (let ((ftype (ffi-type-of arg)))
    (case ftype
      ((<ffi-cos>)        (let ((type (ffi-cos-type-of arg)))
                            (case type
                              ((<ffi-cos-bool>)         (print (ffi-obj-bool arg)))
                              ((<ffi-cos-number>)       (print (ffi-obj-number arg)))
                              ((<ffi-cos-real>)         (print (ffi-obj-real arg)))
                              ((<ffi-cos-string>)       (write (ffi-obj-string arg)))
                              ((<ffi-cos-hexstring>)    (print (ffi-obj-hexstring arg)))
                              ((<ffi-cos-name>)         (print (ffi-obj-name arg)))
                              ((<ffi-cos-array>)        (print-array type arg))
                              ((<ffi-cos-dictionary>)   (print-dict type arg))
                              ((<ffi-cos-rawdata>)      (print type))
                              ((<ffi-cos-reference>)    (print (ffi-ref-id arg)))
                              ((<ffi-cos-stream>)       (print type))
                              ((<ffi-cos-null>)         (print type))
                              (else                     (print type)))))
      ((<ffi-dict>)       (print-dict ftype arg)
      ((<ffi-mem-doc>)    (print ftype))
      ((<ffi-stream-doc>) (print ftype))
      ((<ffi-none>)       (print ftype))
      (else               (print arg))))))

(define (print-dictionary obj)
  (print "   dict. content: ")
  (print-value obj))

(define d1 (ffi-create-mem-doc))
(println "        document: " d1)
(println "         type-of: " (ffi-type-of d1))

(ffi-doc-load d1 "hello.pdf")

(define catalog (ffi-doc-catalog d1))
(println "         catalog: " catalog)
(println "   is dictionary? " (ffi-obj-dictionary? catalog))
(print-dictionary catalog)
(println)

(define trailer (ffi-doc-trailer d1))
(println "         trailer: " trailer)
(println "   is dictionary? " (ffi-obj-dictionary? trailer))
(println "      has stream? " (ffi-obj-stream? trailer))
(print-dictionary trailer)
(println)

(define info (ffi-dic-indirect trailer "Info"))
(println "            info: " info)
(print-dictionary info)
(println)

(define st-root (ffi-doc-struct-tree-root d1))
(println "struct tree root: " st-root)
(println "   is dictionary? " (ffi-obj-dictionary? st-root))
(print-dictionary st-root)
(println)

(define pages (find-object catalog "Pages" "Kids"))
(println "           pages: " pages)

(define page (ffi-arr-indirect pages 0))
(println "          page 1: " page )
(print-dictionary page)
(println)


(define contents (find-object page "Contents"))
(println "        contents: " contents)

(define stream (ffi-dic-filtered-stream contents))
(define data (make-u8vector 126))
(println "            read: " (ffi-stream-read stream data))
(print   "            data: '")
(for-each (lambda (i)
            (print (integer->char (u8vector-ref data i))))
          (iota (u8vector-length data)))
(println "'")
(ffi-dispose d1)
