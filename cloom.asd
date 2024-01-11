(defsystem "cloom"
  :description "command line common lisp cloom"
  :version "1.2"
  :author "ben & simon"
  :serial t
  :components ((:file "settings")
  	       (:file "binary-reader")
	       (:file "wad-types")
	       (:file "wad-reader")
	       (:file "map-data")
	       (:file "angle")
	       (:file "classes")
	       (:file "player")
	       (:file "bsp")
	       (:file "main")))
