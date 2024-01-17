;; player.lisp
(defpackage :player
  (:use :cl :angle :settings))

(in-package :player)

(defmethod get-height (player)
  (setf (height player) (bsp::get-ssector-height (engine::bsp (engine player)))))

(defmethod move (player)
  ;; bewegungshandling cl::charms
  ;; jetzt doch in driver loop
  )
  

(defmethod update (player)
  ;; (move player)
  (get-height player))
  

(defmethod rotate-left (player)
  (incf (angle player) (* 0.2 (PLAYER_ROT_SPEED player))))

(defmethod rotate-right (player)
  (decf (angle player) (* 0.2 (PLAYER_ROT_SPEED player))))

