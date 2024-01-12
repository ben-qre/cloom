;; main.lisp
(in-package :engine)

#|
(defclass CloomEngine ()
  ((map-data :accessor map-data :initarg :map-data)
  (player    :accessor player   :initarg :player)
  (bsp       :accessor bsp      :initarg :bsp)))

(defmethod make-engine (map-name)
  (make-instance 'CloomEngine
		 :map-data (map-data::map-data-init map-name)))

|#

(defmethod engine-init (engine)
  (setf (slot-value engine 'player) (player::make-player engine))
  (setf (slot-value engine 'bsp) (bsp::make-bsp engine))
  engine)
  
