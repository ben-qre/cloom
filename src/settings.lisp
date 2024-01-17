#|
(require 'uiop)
(load "~/.sbclrc")
(ql:quickload :cl-charms)
|#

(defpackage :settings
  (:use :cl)
  (:export :PLAYER_ROT_SPEED :FOV :HALF_FOV :SCREEN_HEIGHT :SCREEN_WIDTH :SCREEN_DIST :HALF_HEIGHT :HALF_WIDTH :MIN_SCALE :MAX_SCALE :PLAYER_HEIGHT))

(in-package :settings)



(defconstant PLAYER_ROT_SPEED 0.12)
(defconstant PLAYER_HEIGHT 30.0)

(defconstant FOV 90.0)
(defconstant HALF_FOV (/ FOV 2))



(defparameter SCREEN_HEIGHT 72)
(defparameter HALF_HEIGHT (/ SCREEN_HEIGHT 2))
(defparameter SCREEN_WIDTH 223)
(defparameter HALF_WIDTH (/ SCREEN_WIDTH 2))
(defparameter SCREEN_DIST (/ HALF_WIDTH (* pi (/ HALF_FOV 180.0))))


(defparameter MIN_SCALE 0.00390625)
(defparameter MAX_SCALE 64.0)

#|
(defun update ()
  (charms:with-curses ()
    (multiple-value-bind (SCREEN_WIDTH SCREEN_HEIGHT)
	(charms:window-dimensions charms:*standard-window*)
      (setf HALF_HEIGHT (floor SCREEN_HEIGHT 2))
      (setf HALF_WIDTH (floor SCREEN_WIDTH 2))
      (setf SCREEN_DIST (/ HALF_WIDTH (* pi (/ HALF_FOV 180.0)))))))
|#
    




