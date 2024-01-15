;; player.lisp
(defpackage :player
  (:use :cl :angle :settings))

(in-package :player)

(defmethod get-height (player)
  (setf (height player) (bsp::get-ssector-height (engine::bsp (engine player)))))

(defmethod move (player)
  ;; bewegungshandling cl::charms
  )
  

(defmethod update (player)
  (move player)
  (get-height player))
  

(defmethod rotate-left (player)
  (incf (angle p) (* 0.2 (rot-speed p))))

(defmethod rotate-right (player)
  (decf (angle p) (* 0.2 (rot-speed p))))

