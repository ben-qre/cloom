(defpackage :bsp
  (:use :common-lisp :wad-types :angle :settings))

(in-package :bsp)

#| from classes.lisp
(defclass bsp ()
  ((engine          :accessor engine          :initarg :engine)
   (player          :accessor player          :initarg :player)
   (nodes           :accessor nodes           :initarg :nodes)
   (ssectors        :accessor ssectors        :initarg :ssectors)
   (segs            :accessor segs            :initarg :segs)
   (root-node-id    :accessor root-node-id)))

(defmethod make-bsp (engine)
  (let ((bsp (make-instance 'bsp
                 :engine engine
                 :player (engine::player engine)
                 :nodes (map-data::nodes (engine::map-data engine))
                 :ssectors (map-data::ssectors (engine::map-data engine))
                 :segs (map-data::segs (engine::map-data engine)))))
    (setf (root-node-id bsp) (- (length (nodes bsp)) 1))
    bsp))
|#

(defmethod update (bsp)
  (setf (is-traverse bsp) t)
  (load-bsp bsp)
  )

(defun angle-to-coordinate (angle)
  (format t "angle to coordinate: ~a~%" angle)
  (if (angle> angle 0.0)
      (progn 
      (format t "true~%")
      (let ((x (- SCREEN_DIST (* (tan (* pi (/ (angle-value angle) 180.0))) HALF_WIDTH))))
	(format t "returning ~a~%" x)
        (truncate x)))
      (let ((x (+ SCREEN_DIST (* (- (tan (* pi (/ (angle-value angle) 180.0)))) HALF_WIDTH))))
	(format t "returning ~a~%" x)
        (truncate x))))

(defun angle-player-to-point (player-instance vertex)
  (let* ((dx (- (wad-types::x vertex) (player::x player-instance)))
	 (dy (- (wad-types::y vertex) (player::y player-instance))))
    (make-angle (* (atan dy dx) (/ 180 PI)))))


(defun is-in-fov (player-instance vertex1 vertex2)
  (format t "checking if in fov~%")
  (let* ((angle1 (angle-player-to-point player-instance vertex1))
	 (angle2 (angle-player-to-point player-instance vertex2))
	 (vertexspan (angle- angle1 angle2)))
    
    (when (angle< vertexspan 180.0) ;; check if next to player or behind
        

      (let* ((r-angle1 (angle= (make-angle 0) angle1))
	     (span1 (angle+ (angle-= angle1 (player::angle player-instance)) HALF_FOV))
	     (span2 (angle- HALF_FOV (angle-= angle2 (player::angle player-instance)))))
	
	(if (angle> span1 FOV)
	    (if (angle< span1 (angle+ vertexspan FOV))
		(angle= angle1 HALF_FOV) ;; cutting v1
		nil))
	
	(if (angle> span2 FOV)
	    (if (angle< span2 (angle+ vertexspan FOV))
		(angle= angle1 (- HALF_FOV)) ;; cutting v2
		nil))
		
	(let ((x1 (angle-to-coordinate angle1))
	      (x2 (angle-to-coordinate angle2)))
	  (format t "x1: ~a    x2: ~a    rw-angle1: ~a~%" x1 x2 r-angle1)
	  (values x1 x2 r-angle1))))))

#| ;; testing
(defun is-in-fov (player-instance vertex1 vertex2)
  (print (list 'player-instance player-instance))
  (print (list 'vertex1 vertex1))
  (print (list 'vertex2 vertex2))

  (let* ((angle1 (angle-player-to-point player-instance vertex1))
         (angle2 (angle-player-to-point player-instance vertex2))
         (vertexspan (angle- angle1 angle2)))

    (print (list 'angle1 angle1))
    (print (list 'angle2 angle2))
    (print (list 'vertexspan vertexspan))

    (if (angle>= vertexspan 180.0) ;; check if next to player or behind
        (return-from is-in-fov nil))

    (let* ((angle1-copy (angle= (make-angle 0) angle1))
           (span1 (angle+ (angle-= angle1 (player::angle player-instance)) HALF_FOV))
           (span2 (angle- HALF_FOV (angle-= angle2 (player::angle player-instance)))))

      (print (list 'angle1-copy angle1-copy))
      (print (list 'span1 span1))
      (print (list 'span2 span2))

      (if (angle> span1 FOV)
          (if (angle>= span1 (angle+ vertexspan FOV))
              (return-from is-in-fov nil)
              ;; else
              (angle= angle1 HALF_FOV))) ;; cutting v1

      (if (angle> span2 FOV)
          (if (angle>= span2 (angle+ vertexspan FOV))
              (return-from is-in-fov nil)
              ;; else
              (angle= angle1 (- HALF_FOV)))) ;; cutting v2

      ;; übergabelogik zum renderer
      )))
      
|#	 

   
(defmethod render-ssector (bsp subsector-id)
  (format t "looking at subsector~%")
  (let ((subsector (nth subsector-id (ssectors bsp))))    
    (format t "subsector-id: ~a~%" subsector-id) ;; testing
    (dotimes (i (wad-types::seg-count subsector))
      (let ((seg (nth (+ (wad-types::first-seg subsector) i) (segs bsp))))
	(format t "v1id: ~a v2id: ~a~%" (wad-types::v1-id seg) (wad-types::v2-id seg)) ;; testing
	(multiple-value-bind (x1 x2 rw-angle1) (is-in-fov (player bsp) (wad-types::v1 seg) (wad-types::v2 seg))
	  (format t "x1: ~a    x2: ~a    rw-angle1: ~a    " x1 x2 rw-angle1)
	  (if (null x1)
	    nil
	    (seghandler::classify-segment (engine::seghandler (engine bsp)) seg x1 x2 rw-angle1)))))))

(defun is-point-on-left-side (x-position y-position node)
  (let* ((dx (- x-position (wad-types::x node)))
         (dy (- y-position (wad-types::y node))))
    (<= (- (* dx (wad-types::dx node))
           (* dy (wad-types::dy node)))
        0)))

(defmethod render-bsp-nodes (bsp node-id)
  (when (is-traverse bsp)
    (if (logbitp 15 node-id) ;;subsector indetifier checking
	(render-ssector bsp (logand node-id #x7FFF))
	;; else
	(let ((lchild (wad-types::lchild (nth node-id (nodes bsp))))
	      (rchild (wad-types::rchild (nth node-id (nodes bsp))))		    
	      (is-on-left (is-point-on-left-side (player::x (player bsp))
						 (player::y (player bsp))
						 (nth node-id (nodes bsp)))))
	  (if is-on-left
	      (progn
		(render-bsp-nodes bsp lchild)
		(render-bsp-nodes bsp rchild))
	      ;; else
	      (progn
       		(render-bsp-nodes bsp lchild)
       		(render-bsp-nodes bsp rchild)))))))

(defmethod load-bsp (bsp)
  (render-bsp-nodes bsp (root-node-id bsp)))
