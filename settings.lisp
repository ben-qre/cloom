#|
(require 'uiop)
(load "~/.sbclrc")
(ql:quickload :cl-charms)
|#

(defpackage :settings
  (:use :cl)
  (:export :PLAYER_ROT_SPEED :FOV :HALF_FOV :SCREEN_HEIGHT :SCREEN_WIDTH :SCREEN_DIST :HALF_HEIGHT :HALF_WIDTH))

(in-package :settings)



(defconstant PLAYER_ROT_SPEED 0.12)

(defconstant FOV 90.0)
(defconstant HALF_FOV (/ FOV 2))

(defparameter SCREEN_HEIGHT 500)
(defparameter HALF_HEIGHT 250)
(defparameter SCREEN_WIDTH 800)
(defparameter HALF_WIDTH 400)
(defparameter SCREEN_DIST (/ HALF_WIDTH (* pi (/ HALF_FOV 180.0))))

#|
(defun update ()
  (charms:with-curses ()
    (multiple-value-bind (SCREEN_WIDTH SCREEN_HEIGHT)
	(charms:window-dimensions charms:*standard-window*)
      (setf HALF_HEIGHT (floor SCREEN_HEIGHT 2))
      (setf HALF_WIDTH (floor SCREEN_WIDTH 2))
      (setf SCREEN_DIST (/ HALF_WIDTH (* pi (/ HALF_FOV 180.0)))))))
|#
    




