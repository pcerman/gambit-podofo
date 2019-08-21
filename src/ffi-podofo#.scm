
;; Copyright (c) 2019 Peter Cerman (https://github.com/pcerman)
;;
;; gambit-podofo: This is gambit scheme (http://gambitscheme.org)
;; binding to the podofo library (http://podofo.sourceforge.net).
;;
;; Original source code if this library is published on github
;; resository (https://github.com/pcerman/gambit-podofo).
;;
;; It is released under GNU General Public License v3.0

(##namespace ("PoDoFo#"

  podofo-version
  ffi-version

  ffi-podofo?
  ffi-dispose
  ffi-type-of
  ffi-cos-type-of

  ffi-create-mem-doc
  ffi-doc-version
  ffi-doc-set-version
  ffi-doc-load
  ffi-doc-write

  ffi-doc-catalog
  ffi-doc-trailer
  ffi-doc-struct-tree-root

  ffi-mem-doc?
  ffi-cos?

  ffi-obj-bool?
  ffi-obj-number?
  ffi-obj-real?
  ffi-obj-string?
  ffi-obj-hexstring?
  ffi-obj-name?
  ffi-obj-array?
  ffi-obj-dictionary?
  ffi-obj-rawdata?
  ffi-obj-null?
  ffi-obj-reference?
  ffi-obj-stream?
  ffi-obj-bool
  ffi-obj-number
  ffi-obj-real
  ffi-obj-string
  ffi-obj-hexstring
  ffi-obj-name

  ffi-ref-id
  ffi-ref-indirect

  ffi-arr-size
  ffi-arr-value
  ffi-arr-indirect
  ffi-arr-values
  ffi-arr-foldl
  ffi-arr-foldr

  ffi-arr-resize
  ffi-arr-remove
  ffi-arr-insert
  ffi-arr-set-bool
  ffi-arr-set-number
  ffi-arr-set-real
  ffi-arr-set-string
  ffi-arr-set-hexstring
  ffi-arr-set-name
  ffi-arr-set-null
  ffi-arr-set-dictionary
  ffi-arr-set-array
  ffi-arr-set-reference

  ffi-dic-size
  ffi-dic-key
  ffi-dic-value
  ffi-dic-indirect
  ffi-dic-keys
  ffi-dic-values
  ffi-dic-foldl
  ffi-dic-foldr

  ffi-dic-remove
  ffi-dic-set-bool
  ffi-dic-set-number
  ffi-dic-set-real
  ffi-dic-set-string
  ffi-dic-set-hexstring
  ffi-dic-set-name
  ffi-dic-set-null
  ffi-dic-set-dictionary
  ffi-dic-set-array
  ffi-dic-set-reference

  ffi-dic-stream
  ffi-dic-filtered-stream
  ffi-dic-set-stream

  ffi-create-stream
  ffi-stream-capacity
  ffi-stream-set-capacity
  ffi-stream-size
  ffi-stream-set-size
  ffi-stream-position
  ffi-stream-set-position
  ffi-stream-available
  ffi-stream-eof?
  ffi-stream-read
  ffi-stream-write
  ffi-stream-print
  ffi-stream-dispose

  ffi-release-count

))
