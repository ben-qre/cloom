(defsystem "wad-loader"
  :description "The WAD file data loader."
  :version "1.0"
  :author "simon"
  :serial t
  :components ((:file "src/binary-reader")
	       (:file "src/wad-types")
	       (:file "src/wad-loader")))
