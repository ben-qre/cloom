(defpackage :wad-types
  (:use :common-lisp :binary-reader)
  (:export :*map-lumps* :int16 :uint8 :uint16 :uint32 :ascii-string :binary-element-list :bbox
	   :wadinfo :filelump :map-data :thing :linedef :vertex :seg :subsector :node :sector))

(in-package :wad-types)


(defvar *map-lumps* (list "THINGS" "LINEDEFS" "SIDEDEFS" "VERTEXES" "SEGS"
			  "SSECTORS" "NODES" "SECTORS" "REJECT" "BLOCKMAP"))

(define-binary-type generic-int in (bytes unsigned)
  (let ((value 0))
    (dotimes (i bytes)
      (setf value (logior (ash (read-byte in) (* i 8)) value)))
    (if (or unsigned (< value (ash 1 (1- (* bytes 8)))))
	value
	(- value (ash 1 (* bytes 8))))))

(define-binary-type  int16 in () (read-value 'generic-int in :bytes 2 :unsigned nil))
(define-binary-type uint8  in () (read-value 'generic-int in :bytes 1 :unsigned t))
(define-binary-type uint16 in () (read-value 'generic-int in :bytes 2 :unsigned t))
(define-binary-type uint32 in () (read-value 'generic-int in :bytes 4 :unsigned t))

(define-binary-type binary-element-list in (length (element-type 'uint8))
  (let ((list '()))
    (dotimes (i length)
      (setf list (cons (read-value element-type in) list)))
    (setf list (nreverse list))))

(define-binary-type ascii-string in (length)
  (with-output-to-string (s)
    (dotimes (i length)
      (let ((code (read-byte in)))
	(if (not (zerop code))
	    (write-char (code-char code) s))))))

(define-binary-element-class bbox
  ((yu (int16))
   (yl (int16))
   (xl (int16))
   (xu (int16))))


(define-binary-element-class wadinfo
  ((identification (ascii-string :length 4))
   (numlumps       (uint32))
   (infotableofs   (uint32))))

(define-binary-element-class filelump
  ((filepos (uint32))
   (size    (uint32))
   (name    (ascii-string :length 8))))


(defclass map-data ()
  ((things   :accessor things)
   (linedefs :accessor linedefs)
   (vertexes :accessor vertexes)
   (segs     :accessor segs)
   (ssectors :accessor ssectors)
   (nodes    :accessor nodes)
   (sectors  :accessor sectors)))


(define-binary-element-class thing
  ((x      (int16))
   (y      (int16))
   (angle  (int16))
   (t-type (int16))
   (flags  (int16))))

(define-binary-element-class linedef
  ((v1       (uint16))
   (v2       (uint16))
   (flags     (int16))
   (special_t (int16))
   (tag       (int16))
   (sidenum1  (int16))
   (sidenum2  (int16))))

(define-binary-element-class vertex
  ((x (int16))
   (y (int16))))

(define-binary-element-class seg
  ((v1    (int16))
   (v2    (int16))
   (angle (int16))
   (l-id  (int16))
   (dirct (int16))
   (offs  (int16))))

(define-binary-element-class subsector
  ((seg-count (uint16))
   (first-seg (uint16))))

(define-binary-element-class node
  ((x      (int16))
   (y      (int16))
   (dx     (int16))
   (dy     (int16))
   (rbox   (bbox))
   (lbox   (bbox))
   (rchild (int16))
   (lchild (int16))))

(define-binary-element-class sector
  ((floorheight   (int16))
   (ceilingheight (int16))
   (floorflat     (ascii-string :length 8))
   (ceilingflat   (ascii-string :length 8))
   (lightlevel    (int16))
   (s-type        (int16))
   (tag           (int16))))
