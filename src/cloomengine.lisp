;; cloomengine.lisp

(in-package :engine)

(defmethod engine-init (engine)
  (setf (slot-value engine 'clock) (clock::make-clock engine))
  (setf (slot-value engine 'player) (player::make-player engine))
  (setf (slot-value engine 'bsp) (bsp::make-bsp engine))
  (setf (slot-value engine 'seghandler) (seghandler::make-seghandler engine))
  (setf (slot-value engine 'view-renderer) (view-renderer::view-renderer-init))
  ;; (format t "cloom engine initialized~%")
  engine)

(defmethod update (engine)
  (player::update (player engine))
  (seghandler::update (seghandler engine))
  (bsp::update (bsp engine))
  (view-renderer::update (view-renderer engine))
  (setf (time-delta engine) (clock::tick (clock engine) 60)))

(defmethod run (engine)
  (loop :named driver-loop :for c := (charms:get-char (view-renderer::window (view-renderer engine)) :ignore-error t)
	:do (progn
	      (case c
		(#\p (return-from driver-loop))
		(#\w (setf (player::y (player engine)) (+ (player::y (player engine)) 5)))
		(#\s (setf (player::y (player engine)) (- (player::y (player engine)) 5)))
		(#\a (setf (player::x (player engine)) (- (player::x (player engine)) 5)))
		(#\d (setf (player::x (player engine)) (+ (player::x (player engine)) 5)))
		(#\q (player::rotate-left (player engine)))
		(#\e (player::rotate-right (player engine)))
		(-1  nil))
	      
	      (update engine)))
	      
  (view-renderer::view-renderer-close))
