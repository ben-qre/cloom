(defsystem "cloom"
  :description "command line common lisp cloom"
  :version "1.2"
  :author "ben & simon"
  :serial t
  :components ((:file "src/settings")
  	       (:file "src/binary-reader")
	       (:file "src/wad-types")
	       (:file "src/wad-reader")
	       (:file "src/map-data")
	       (:file "src/angle")
	       (:file "src/classes")
	       (:file "src/seghandler")
	       (:file "src/player")
	       (:file "src/bsp")
	       (:file "src/main")))
