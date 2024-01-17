;; main.lisp
(require 'uiop)
(load "~/.sbclrc")
(ql:quickload :cl-charms)
(ql:quickload "cloom")

(defun initialize-cloom ()
  (let ((engine (engine::engine-init (engine::make-engine "E1M1"))))
    (engine::run engine)))

(defun main ()
  (initialize-cloom))

(sb-ext:save-lisp-and-die "cloom.exe" :toplevel #'main :executable t)
