;; player.lisp
(defpackage :player
  (:use :cl :angle :settings))

(in-package :player)

#| from classes.lisp
(defclass player ()
  ((engine  :initarg :engine    :accessor engine)
   (x       :initarg :x         :accessor x)
   (y       :initarg :y         :accessor y)
   (rot     :initarg :rot-speed :accessor rot-speed)
   (angle   :initarg :angle     :accessor angle)))

(defun make-player (engine)
  (let ((start-info (nth 0 (map-data::things (engine::map-data engine)))))
    (make-instance 'player
		   :engine engine
                   :x (wad-types::x start-info)
                   :y (wad-types::y start-info)
                   :rot-speed PLAYER_ROT_SPEED
                   :angle (angle::make-angle (wad-types::angle start-info)))))

|#

(defmethod move ((p player))
  ;; bewegungshandling cl::charms
  )
  

(defmethod tick ((p player))
  (move p)
  )
  

(defmethod rotate-left ((p player))
  (incf (angle p) (* 0.2 (rot-speed p))))

(defmethod rotate-right ((p player))
  (decf (angle p) (* 0.2 (rot-speed p))))

