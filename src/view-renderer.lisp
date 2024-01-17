(defpackage :view-renderer
  (:use :common-lisp :wad-reader)
  (:export :view-renderer :view-renderer-init :update :view-renderer-close :draw-vline :window-dimensions))

(in-package :view-renderer)

(defconstant MAX_COLORS 256)

(defclass view-renderer ()
  ((window  :accessor window)
   (winptr  :accessor winptr)
   (width   :accessor width)
   (height  :accessor height)))


(defmethod window-dimensions (view-renderer)
  (let (width height)
    (charms/ll:getmaxyx (winptr view-renderer) height width)
    (values width height)))

(defmethod update-window-dimensions (view-renderer)
  (with-slots (winptr width height) view-renderer
    (charms/ll:getmaxyx winptr height width)
    (setf settings::SCREEN_HEIGHT height)
    (setf settings::SCREEN_WIDTH width)
    (setf settings::HALF_HEIGHT (/ settings::SCREEN_HEIGHT 2))
    (setf settings::HALF_WIDTH (/ settings::SCREEN_WIDTH 2))))
    

(defun scale (val min1 max1 min2 max2)
  (let ((scale1 (- max1 min1))
	(scale2 (- max2 min2)))
    (+ (* (/ (- val min1) scale1) scale2) min2)))

(defmacro define-scale-function (name min1 max1 min2 max2 round)
  `(defun ,name (value)
     (let ((new-val (scale value ,min1 ,max1 ,min2 ,max2)))
       (if ,round
	   (round new-val)
	   new-val))))

(define-scale-function scale-rgb-to-ncurses 0 255 0 1000 t)

(defun create-colors (palette)
  (dotimes (i MAX_COLORS)
    (with-slots (wad-types::r wad-types::g wad-types::b) (nth i palette)
      (let ((r (scale-rgb-to-ncurses wad-types::r))
	    (g (scale-rgb-to-ncurses wad-types::g))
	    (b (scale-rgb-to-ncurses wad-types::b)))
	(charms/ll:init-color i r g b)))))

(defun create-color-palette (palette font-color-index)
  (create-colors palette)
  (dotimes (i MAX_COLORS)
    (charms/ll:init-pair i font-color-index i)))
    
(defun load-color-palette (palette-index font-color-index)
  (let ((palette (wad-reader:get-color-palette palette-index)))
    (create-color-palette palette font-color-index)))

(defun color-init ()
  (when (eql charms/ll:true (charms/ll:has-colors))
    (charms/ll:start-color)
    (load-color-palette 0 0)))

(defun view-renderer-init ()
  (let ((renderer (make-instance 'view-renderer)))
    (with-slots (window winptr) renderer
      (setf window (charms:initialize))
      (setf winptr (charms::window-pointer window))
      (charms:enable-non-blocking-mode window))
    (charms/ll:curs-set charms/ll:false)
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (color-init)
    (update-window-dimensions renderer)
    renderer))

(defun view-renderer-close ()
  (charms/ll:getch)
  (charms:finalize))


(defmacro with-color ((winptr color-index) &body body)
  `(let ((index (mod ,color-index MAX_COLORS)))
     (charms/ll:wattron  ,winptr (charms/ll:color-pair ,color-index))
     ,@body
     (charms/ll:wattroff ,winptr (charms/ll:color-pair ,color-index))))

(defmacro defun-grayscale (name grayscale)
  `(defun ,name (lightlevel)
     (let ((index (truncate (* (/ lightlevel 256.0) ,(length grayscale)))))
       (elt ,grayscale index))))

(defun-grayscale short-grayscale1 "@#%+=*:-. ")
(defun-grayscale short-grayscale2 "$EFLlv!;,. ")
(defun-grayscale long-grayscale "$@B%8&WM#oahkbdpqwmZO0QLCJUYXzcvunxrjft/\|()1{}[]?-_+~<>i!lI;:,^`'. ")

(defun texture-to-random-color (texture-name seed)
  (abs (+ seed (sxhash texture-name))))

(defmethod draw-vline (view-renderer x y1 y2 texture lightlevel)
  (with-slots (window winptr) view-renderer
    (let ((color-id (mod (texture-to-random-color texture 50) MAX_COLORS))
	  (char     (short-grayscale1 (mod lightlevel 256))))
      (with-color (winptr color-id)
	(loop for y from y1 to y2
	      do (charms/ll:mvwaddch winptr y x (char-code char)))))))

(defmethod update (view-renderer)
  (charms:refresh-window (window view-renderer))
  (update-window-dimensions view-renderer))
