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

(defun angle-to-coordinate (angle)
  (if (angle> angle 0)
      (let ((x (- SCREEN_DIST (* (tan (* pi (/ angle 180.0))) HALF_WIDTH))))
        (truncate x))
      (let ((x (+ SCREEN_DIST (* (- (tan (* pi (/ angle 180.0)))) HALF_WIDTH))))
        (truncate x))))

(defun angle-player-to-point (player-instance vertex)
  (let* ((dx (- (wad-types::x vertex) (player::x player-instance)))
	 (dy (- (wad-types::y vertex) (player::y player-instance))))
    (make-angle (* (atan dy dx) (/ 180 PI)))))


(defun is-in-fov (player-instance vertex1 vertex2)
  (let* ((angle1 (angle-player-to-point player-instance vertex1))
	 (angle2 (angle-player-to-point player-instance vertex2))
	 (vertexspan (angle- angle1 angle2)))
    
    (if (angle>= vertexspan 180.0) ;; check if next to player or behind
        (return-from is-in-fov nil))

    (let* ((angle1-copy (angle= (make-angle 0) angle1))
	   (span1 (angle+ (angle-= angle1 (player::angle player-instance)) HALF_FOV))
	   (span2 (angle- HALF_FOV (angle-= angle2 (player::angle player-instance)))))
      
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

   
(defun render-ssector (bsp-instance subsector-id)
  (let ((subsector (nth subsector-id (ssectors bsp-instance))))
    (format t "subsector-id: ~a~%" subsector-id) ;; testing
    (dotimes (i (wad-types::seg-count subsector))
      (let ((seg (nth (+ (wad-types::first-seg subsector) i) (segs bsp-instance))))
	(format t "v1id: ~a v2id: ~a~%" (wad-types::v1-id seg) (wad-types::v2-id seg)) ;; testing
	(is-in-fov (player bsp-instance) (wad-types::v1 seg) (wad-types::v2 seg))
	;; render logik
	;; case checking ob im fov
	;; is-in-fov
	;; renderer anknüpfung
	))
    ))

(defun is-point-on-left-side (x-position y-position node)
  (let* ((dx (- x-position (wad-types::x node)))
         (dy (- y-position (wad-types::y node))))
    (<= (- (* dx (wad-types::dx node))
           (* dy (wad-types::dy node)))
        0)))

(defun render-bsp-nodes (bsp-instance node-id)
  (if (logbitp 15 node-id) ;;subsector indetifier checking
      (render-ssector bsp-instance (logand node-id #x7FFF))
      ;else
      (let ((lchild (wad-types::lchild (nth node-id (nodes bsp-instance))))
	    (rchild (wad-types::rchild (nth node-id (nodes bsp-instance))))		    
	    (is-on-left (is-point-on-left-side (player::x (player bsp-instance))
					       (player::y (player bsp-instance))
					       (nth node-id (nodes bsp-instance)))))
	(if is-on-left
	    (progn
	      (render-bsp-nodes bsp-instance lchild)
	      (render-bsp-nodes bsp-instance rchild))
	    ;else
	    (progn
       	      (render-bsp-nodes bsp-instance lchild)
       	      (render-bsp-nodes bsp-instance rchild))))))

(defun load-bsp (bsp-instance)
  (render-bsp-nodes bsp-instance (root-node-id bsp-instance)))
