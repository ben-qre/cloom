(defpackage :map-data
  (:use :common-lisp :wad-reader)
  (:export :map-data :map-data-init))

(in-package :map-data)


(defclass map-data ()
  ((things   :accessor things)
   (linedefs :accessor linedefs)
   (sidedefs :accessor sidedefs)
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
      (set-map-slot sidedefs get-sidedefs)
      (set-map-slot vertexes get-vertexes)
      (set-map-slot segs     get-segs)
      (set-map-slot ssectors get-ssectors)
      (set-map-slot nodes    get-nodes)
      (set-map-slot sectors  get-sectors))
    map))

(defmethod update-linedefs (map-data)
  (with-slots (linedefs sidedefs) map-data
    (dolist (linedef linedefs)
      (with-slots (wad-types::side-id1  wad-types::side-id2
		   wad-types::frontside wad-types::backside) linedef
	(setf wad-types::frontside (nth wad-types::side-id1 sidedefs))
	(let ((backside))
	  (if (not (eql wad-types::side-id2 #xFFFF))
	      (setf backside (nth wad-types::side-id2 sidedefs)))
	  (setf wad-types::backside backside))))))

(defmethod update-sidedefs (map-data)
  (with-slots (sidedefs sectors) map-data
    (dolist (sidedef sidedefs)
      (with-slots (wad-types::sec-id wad-types::sector) sidedef
	(setf wad-types::sector (nth wad-types::sec-id sectors))))))

(defun bams-to-degrees (binary-angle)
  (let ((angle (* (ash binary-angle 16) 8.38190317e-8)))
    (if (< angle 0)
	(+ angle 360)
	angle)))

(defmethod update-segs (map-data)
  (with-slots (linedefs vertexes segs) map-data
    (dolist (seg segs)
      ; dereference ids
      (with-slots (wad-types::v1-id wad-types::v2-id wad-types::line-id
		   wad-types::v1    wad-types::v2    wad-types::ldef) seg
	(setf wad-types::v1   (nth wad-types::v1-id   vertexes))
	(setf wad-types::v2   (nth wad-types::v2-id   vertexes))
	(setf wad-types::ldef (nth wad-types::line-id linedefs)))
      
      ; add frontside-/backside-sector to segment
      (with-slots (wad-types::directn wad-types::fsector wad-types::bsector wad-types::ldef) seg
	(let ((front-sidedef)
	      (back-sidedef))
	  (with-slots (wad-types::frontside wad-types::backside) wad-types::ldef
	    (if (not (= 0 wad-types::directn))
		(progn (setf front-sidedef wad-types::backside)
		       (setf back-sidedef  wad-types::frontside))
		(progn (setf front-sidedef wad-types::frontside)
		       (setf back-sidedef  wad-types::backside))))

	  (setf wad-types::fsector (slot-value front-sidedef 'wad-types::sector))
	  (if (string= "TWO_SIDED" (wad-types:linedef-flag (slot-value wad-types::ldef 'wad-types::flags) :name))
	      (setf wad-types::bsector (slot-value back-sidedef 'wad-types::sector))
	      (setf wad-types::bsector nil))))

      ; BAMS to degrees
      (with-slots (wad-types::angle) seg
	(setf wad-types::angle (bams-to-degrees wad-types::angle))))))

(defmethod update-data (map-data)
  (update-linedefs map-data)
  (update-sidedefs map-data)
  (update-segs     map-data))

(defmethod map-data-init (map-name)
  (let* ((reader (wad-reader-init "../data/DOOM1.WAD"))
	 (map-id (get-lump-index reader map-name))
	 (map    (get-map-data   reader map-id)))
    (update-data map)
    (wad-reader-close reader)
    map))
