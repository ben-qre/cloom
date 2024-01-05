(defsystem "cloom"
  :description "command line common lisp cloom"
  :version "1.1"
  :author "ben & simon"
  :serial t
  :components ((:file "src/binary-reader")
	       (:file "src/wad-types")
	       (:file "src/wad-reader")
	       (:file "src/map-data")))
