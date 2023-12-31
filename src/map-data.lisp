(defpackage :map-data
  (:use :common-lisp :wad-reader)
  (:export :map-data :map-data-init))

(in-package :map-data)


(defclass map-data ()
  ((things   :accessor things)
   (linedefs :accessor linedefs)
   (vertexes :accessor vertexes)
   (segs     :accessor segs)
   (ssectors :accessor ssectors)
   (nodes    :accessor nodes)
   (sectors  :accessor sectors)))

(defmacro set-map-slot (slot-name get-function)
  `(setf (slot-value map ',slot-name) (,get-function wad-reader map-index)))

(defmethod get-map-data (wad-reader map-index)
  (let ((map (make-instance 'map-data)))
    (with-slots (things linedefs vertexes segs ssectors nodes sectors) map
      (set-map-slot things   get-things)
      (set-map-slot linedefs get-linedefs)
      (set-map-slot vertexes get-vertexes)
      (set-map-slot segs     get-segs)
      (set-map-slot ssectors get-ssectors)
      (set-map-slot nodes    get-nodes)
      (set-map-slot sectors  get-sectors))
    map))

(defmethod update-segs (map-data)
  (with-slots (linedefs vertexes segs) map-data
    (dolist (seg segs)
      (with-slots (wad-types::v1-id wad-types::v2-id wad-types::l-id
		   wad-types::v1    wad-types::v2    wad-types::ldef) seg
	(setf wad-types::v1   (nth wad-types::v1-id vertexes))
	(setf wad-types::v2   (nth wad-types::v2-id vertexes))
	(setf wad-types::ldef (nth wad-types::l-id  linedefs))))))

;(defmethod update-linedefs (map-data))
;(defmethod update-sidedefs (map-data))

(defmethod update-data (map-data)
  ;(update-linedefs map-data) ----> and
  ;(update-sidedefs mpa-data) ----> when implementing sidedef (texture) handling
  (update-segs     map-data))

(defmethod map-data-init (map-name)
  (let* ((reader (wad-reader-init "../data/DOOM1.WAD"))
	 (map-id (get-lump-index reader map-name))
	 (map    (get-map-data   reader map-id)))
    (update-data map)
    (wad-reader-close reader)
    map))
