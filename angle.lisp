;; angle.lisp
(defpackage :angle
  (:use :cl)
  (:export :make-angle :angle-value
	   :angle- :angle+ :angle= :angle< :angle<= :angle> :angle>= :-angle :angle+= :angle-= ))

(in-package :angle)

(defclass angle ()
  ((value :initarg :value :accessor angle-value :initform 0)))

(defmethod normalize360 ((angle angle))
  (setf (angle-value angle) (mod (angle-value angle) 360.0))
  angle)

(defun make-angle (initial-value)
  (let ((a (make-instance 'angle :value initial-value)))
    (normalize360 a)))

(defmacro def-arithoperator-angle= (op funct)
  `(defgeneric ,(intern (format nil "ANGLE~a" op)) (lhs rhs)
     (:method ((lhs angle) (rhs float))
       (,funct (angle-value lhs) rhs)
       (normalize360 lhs))
     (:method ((lhs angle) (rhs angle))
       (,funct (angle-value lhs) (angle-value rhs))
       (normalize360 lhs))
     (:method ((lhs float) (rhs angle))
       (,funct lhs (angle-value rhs)))))

(def-arithoperator-angle= = setf)
(def-arithoperator-angle= += incf)
(def-arithoperator-angle= -= decf)

(defmacro def-arithoperator-angle (op)
  `(defgeneric ,(intern (format nil "ANGLE~a" op)) (lhs rhs)
     (:method ((lhs angle) (rhs float))
       (make-angle (,op (angle-value lhs) rhs)))
     (:method ((lhs angle) (rhs angle))
       (make-angle (,op (angle-value lhs) (angle-value rhs))))
     (:method ((lhs float) (rhs angle))
       (make-angle (,op lhs (angle-value rhs))))))

(def-arithoperator-angle +)
(def-arithoperator-angle -)
(def-arithoperator-angle *)
(def-arithoperator-angle /)

(defmacro def-booloperator-angle (op)
  `(defgeneric ,(intern (format nil "ANGLE~a" op)) (lhs rhs)
  (:method ((lhs angle) (rhs float))
    (,op (angle-value lhs) rhs))
  (:method ((lhs angle) (rhs angle))
    (,op (angle-value lhs) (angle-value rhs)))))

(def-booloperator-angle <)
(def-booloperator-angle <=)
(def-booloperator-angle >)
(def-booloperator-angle >=)

(defmethod -angle ((angle angle))
  (setf (angle-value angle) (angle- 0.0 angle))
  (normalize360 angle))

#|
;; fürs probieren:
(defun test ()
  (let ((a1 (make-angle 10.0))
	(a2 (make-angle 90.0)))
    (format t "a1 = ~a~%" (angle-value a1))
    (format t "a2 = ~a~%" (angle-value a2))
    (angle- a1 a2)
    (-angle a2)
    (format t "a1 = ~a~%" (angle-value a1))
    (format t "a2 = ~a~%" (angle-value a2))))

|#
