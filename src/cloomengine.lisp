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
  ;; (setf (time-delta engine) (clock::tick (clock engine) 60))
  )

(defmethod run (engine)
  (update engine)
  ;; (view-renderer::draw-vline (view-renderer engine) 5 10 20 "BEN" 255)
  ;; (view-renderer::update (view-renderer engine))
  (view-renderer::view-renderer-close)
  )
