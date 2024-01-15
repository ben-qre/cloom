;; seghandler.lisp

(in-package :seghandler)

(defun radians (degrees)
  (* degrees (/ pi 180)))

(defun degrees (radians)
  (* radians (/ 180 pi)))

(defun dist (x y vertex)
  (let ((dx (- (wad-types::x vertex) x))
	(dy (- (wad-types::y vertex) y)))
    (sqrt (+ (* dx dx) (* dy dy)))))
  

(defmacro with-bindings (seghandler type &body body)
  (let ((conditional-bindings
	  (if (string= type "solid")
	   `((wall-tex (wad-types::middle side))
	     ;; render booleans
	     (b-draw-wall (not (equal (wad-types::middle side) "-")))
	     (b-draw-ceil (> fsector-z1 0))
	     (b-draw-floor (< fsector-z2 0)))
	   `((bsector (wad-types::bsector segment))
	     (u-wall-tex (wad-types::upper side))
	     (l-wall-tex (wad-types::lower side))
	     (bsector-z1
	      (- (wad-types::ceilingheight bsector) (player::height (player ,seghandler))))
	     (bsector-z2
	      (- (wad-types::floorheight bsector) (player::height (player ,seghandler))))
	     (b-draw-ceil nil)
	     (b-draw-floor nil)
	     (b-draw-uwall nil)
	     (b-draw-lwall nil)
	     (portal-y1 nil)
	     (portal-y1-step nil)
	     (portal-y2 nil)
	     (portal-y2-step nil)
	     (draw-uwall-y1 nil)
	     (draw-uwall-y2 nil)
	     (draw-lwall-y1 nil)
	     (draw-lwall-y2 nil)))))
  	  
    `(let* ((segment (seg ,seghandler))
	    (fsector (wad-types::fsector segment))
	    (ldef (wad-types::ldef segment))
	    (side (wad-types::frontside ldef))
	    ;; (renderer (view-renderer (engine ,seghandler)))
	    (upper-clip (upper-clip ,seghandler))
	    (lower-clip (lower-clip ,seghandler))
	    
	    ;; textures
	    (ceil-tex (wad-types::ceilingflat fsector))
	    (floor-tex (wad-types::floorflat fsector))
	    (light-level (wad-types::lightlevel fsector))
	    
	    (fsector-z1
	      (- (wad-types::ceilingheight fsector) (player::height (player ,seghandler))))
	    (fsector-z2
	      (- (wad-types::floorheight fsector) (player::height (player ,seghandler))))	
	    
	    ;; scaling factors
	    (r-normal-angle (angle+ (make-angle (wad-types::angle segment)) 90))
	    (offset-angle (angle- r-normal-angle (rw-angle1 ,seghandler)))
	    
	    (hypo (dist (wad-types::x (player ,seghandler))
			(wad-types::y (player ,seghandler))
			(wad-types::v1 segment)))
	    
	    (r-distance (* hypo (cos (radians offset-angle))))
	    
	    (r-scale1 (calculate-perspective-scale ,seghandler x1 r-normal-angle r-distance))
	    (r-scale2 (calculate-perspective-scale ,seghandler x2 r-normal-angle r-distance))
	    (r-scale-step 0)

	    ;; for drawing
	    (wall-y1 (- HALF_HEIGHT (* fsector-z1 r-scale1)))
	    (wall-y1-step 0)
	    (wall-y2 (- HALF_HEIGHT (* fsector-z2 r-scale1)))
	    (wall-y2-step 0)
	    (draw-wall-y1 0)
	    (draw-wall-y2 0)
	    ,@conditional-bindings)
       ,@body)))
 
  

(defmethod init-floor-ceil-clip-height (seghandler)
  (setf (upper-clip seghandler) (loop repeat SCREEN_WIDTH collect -1))
  (setf (lower-clip seghandler) (loop repeat SCREEN_WIDTH collect SCREEN_HEIGHT)))

(defmethod update (seghandler)
  (init-floor-ceil-clip-height seghandler)
  (setf (screen-range seghandler) (loop for i from 0 to SCREEN_WIDTH collect i)))

(defmethod calculate-perspective-scale (seghandler x r-normal-angle r-distance)
  (let* ((x-angle (nth x (angles seghandler)))
         (num (* SCREEN_DIST (cos (radians (- r-normal-angle x-angle (player::angle (player seghandler)))))))
         (den (* r-distance (cos (radians x-angle))))
         (scale (/ num den)))
    (max MIN_SCALE (min MAX_SCALE scale))))

(defmethod draw-solid-wall (seghandler x1 x2)
  (format t "drawing solid wall~%")
  (with-bindings seghandler "solid"
    (if (< x1 x2)
	(setf r-scale-step (/ (- r-scale2 r-scale1) (- x1 x2)))
	(setf r-scale-step 0))
    (setf wall-y1-step (* (- r-scale-step) fsector-z1))
    (setf wall-y2-step (* (- r-scale-step) fsector-z2))

    ;; rendering
    (loop for x from x1 to x2 do
      (setf draw-wall-y1 (- wall-y1 1))
      (setf draw-wall-y2 wall-y2)
      
      (when b-draw-ceil
	(let ((cy1 (+ (aref upper-clip x) 1))
	      (cy2 (min (- draw-wall-y1 1) (- (aref lower-clip x) 1))))
	  (format t "cy1: ~a      cy2: ~a~%" cy1 cy2))) ;; testing
	  ;; (renderer::draw-vline renderer x cy1 cy2 ceil-tex light-level))
      
      (when b-draw-wall
	(let ((wy1 (max draw-wall-y1 (+ (aref upper-clip x) 1)))
	      (wy2 (min draw-wall-y2 (- (aref lower-clip x) 1))))
	  (format t "wy1: ~a      wy2: ~a~%" wy1 wy2))) ;; testing
	  ;; (renderer::draw-vline renderer x wy1 wy2 wall-tex light-level))
      
      (when b-draw-floor
	(let ((fy1 (max (+ draw-wall-y2 1) (+ (aref upper-clip x) 1)))
	      (fy2 (- (aref lower-clip x) 1)))
	  (format t "fy1: ~a      fy2: ~a~%" fy1 fy2))) ;; testing
	  ;; (renderer::draw-vline renderer x fy1 fy2 floor-tex light-level))
      
      (setf wall-y1 (+ wall-y1 wall-y1-step))
      (setf wall-y2 (+ wall-y2 wall-y2-step)))))

(defmethod draw-portal-wall (seghandler x1 x2)
  (format t "drawing portal wall~%")
  (with-bindings seghandler "portal"
    (if (< x1 x2)
	(setf r-scale-step (/ (- r-scale2 r-scale1) (- x1 x2)))
	(setf r-scale-step 0))
    (setf wall-y1-step (* (- r-scale-step) fsector-z1))
    (setf wall-y2-step (* (- r-scale-step) fsector-z2))

    ;; draw checking
    ;; for upper wall and ceiling
    (if (or (/= fsector-z1 bsector-z1)
            (/= light-level (wad-types::lightlevel bsector))
            (not (string= ceil-tex (wad-types::ceilingflat bsector))))
	(progn
          (setf b-draw-uwall (and (not (string= (wad-types::upper side) "-")) (< bsector-z1 fsector-z1)))
          (setf b-draw-ceil (>= fsector-z1 0)))
	(progn
          (setf b-draw-uwall nil)
          (setf b-draw-ceil nil)))

    ;; for lower wall and floor
    (if (or (/= fsector-z2 bsector-z2)
            (/= light-level (wad-types::lightlevel bsector))
            (not (string= floor-tex (wad-types::floorflat bsector))))
	(progn
          (setf b-draw-lwall (and (not (string= (wad-types::upper side) "-")) (< bsector-z1 fsector-z1)))
          (setf b-draw-floor (<= fsector-z2 0)))
	(progn
          (setf b-draw-lwall nil)
          (setf b-draw-floor nil)))

    (if (and (not b-draw-uwall) 
             (not b-draw-ceil) 
             (not b-draw-lwall) 
             (not b-draw-floor))
	(return-from draw-portal-wall))

    
    ;; portal position
    (when b-draw-uwall
      (if (> bsector-z1 fsector-z2)
	  (progn
            (setf portal-y1 (- HALF_HEIGHT (* bsector-z1 r-scale1)))
            (setf portal-y1-step (* (- r-scale-step) bsector-z1)))
	  (progn
            (setf portal-y1 wall-y2)
            (setf portal-y1-step wall-y2-step))))

    (when b-draw-lwall
      (if (< bsector-z2 fsector-z1)
	  (progn
            (setf portal-y2 (- HALF_HEIGHT (* bsector-z2 r-scale1)))
            (setf portal-y2-step (* (- r-scale-step) bsector-z2)))
	  (progn
            (setf portal-y2 wall-y1)
            (setf portal-y2-step wall-y1-step))))

    ;; rendering
    (loop for x from x1 to x2 do
      (setf draw-wall-y1 (- wall-y1 1))
      (setf draw-wall-y2 wall-y2)

      ;; process upper wall
      (when b-draw-uwall
	(setf draw-uwall-y1 (- wall-y1 1))
	(setf draw-uwall-y2 portal-y1)

	;; draw ceiling
	(when b-draw-ceil
	  (let ((cy1 (+ (aref upper-clip x) 1))
		(cy2 (min (- draw-wall-y1 1) (- (aref lower-clip x) 1))))
            ;; (renderer-draw-vline renderer x cy1 cy2 tex-ceil-id light-level))
	    (format t "cy1: ~a      cy2: ~a~%" cy1 cy2))) ;; testing


	;; draw upper wall
	(let ((wy1 (max draw-uwall-y1 (+ (aref upper-clip x) 1)))
              (wy2 (min draw-uwall-y2 (- (aref lower-clip x) 1))))
	  ;; (renderer-draw-vline renderer x wy1 wy2 upper-wall-texture light-level))
	  (format t "wy1: ~a      wy2: ~a~%" wy1 wy2) ;; testing

	  (when (< (aref upper-clip x) wy2)
	    (setf (aref upper-clip x) wy2)))

	(incf portal-y1 portal-y1-step))

      ;; draw ceiling alone
      (when b-draw-ceil
	(let ((cy1 (+ (aref upper-clip x) 1))
	      (cy2 (min (- draw-wall-y1 1) (- (aref lower-clip x) 1))))
          ;; (renderer-draw-vline renderer x cy1 cy2 tex-ceil-id light-level))
	  (format t "cy1: ~a      cy2: ~a~%" cy1 cy2) ;; testing
	
	  (when (< (aref upper-clip x) cy2)
	    (setf (aref upper-clip x) cy2))))
        

      ;; process lower wall
      (when b-draw-lwall
	
	;; draw floor
	(when b-draw-floor
	  (let ((fy1 (max (+ draw-wall-y2 1) (+ (aref upper-clip x) 1)))
		(fy2 (- (aref lower-clip x) 1)))
            ;; (renderer-draw-vline renderer x fy1 fy2 tex-floor-id light-level))
	    (format t "fy1: ~a      fy2: ~a~%" fy1 fy2))) ;; testing

	(setf draw-lwall-y1 (- portal-y2 1))
	(setf draw-lwall-y2 wall-y2)

	(let ((wy1 (max draw-lwall-y1 (+ (aref upper-clip x) 1)))
	      (wy2 (min draw-lwall-y2 (- (aref lower-clip x) 1))))
	  ;; (renderer-draw-vline renderer x wy1 wy2 lower-wall-texture light-level))
	  (format t "wy1: ~a      wy2: ~a~%" wy1 wy2) ;; testing
	      
	  (when (> (aref lower-clip x) wy1)
	    (setf (aref lower-clip x) wy1)))
	
	(incf portal-y2 portal-y2-step))

      (when b-draw-floor
	(let ((fy1 (max (+ draw-wall-y2 1) (+ (aref upper-clip x) 1)))
              (fy2 (- (aref lower-clip x) 1)))
	  ;; (renderer-draw-vline renderer x fy1 fy2 tex-floor-id light-level))
	  (format t "fy1: ~a      fy2: ~a~%" fy1 fy2) ;; testing
	
	  (when (> (aref lower-clip x) (+ draw-wall-y2 1))
            (setf (aref lower-clip x) fy1))))

      
      (incf wall-y1 wall-y1-step)
      (incf wall-y2 wall-y2-step))))


(defmethod clip-solid-wall (seghandler x-start x-end)
  (format t "clipping solid wall~%")
  (if (screen-range seghandler)
      (progn 
	(let* ((curr-wall (loop for i from x-start below x-end collect i))
               (intersection (intersection curr-wall (screen-range seghandler))))
          (if (= (length intersection) (length curr-wall))
	      (draw-solid-wall seghandler x-start (- x-end 1))
	      (progn 
		(let* ((arr (sort intersection #'<))
                       (x (first arr))
                       (x2 (last arr)))
		  (loop for x1 in arr and x2 in (cdr arr) do
                    (if (> (- x2 x1) 1)
			(progn
			  (draw-solid-wall seghandler x x1)
			  (setf x x2))))
		  (draw-solid-wall seghandler x x2))
		(setf (screen-range seghandler) (set-difference (screen-range seghandler) intersection))))))
      (setf (bsp::is-traverse (engine::bsp (engine seghandler))) nil)))

(defmethod clip-portal-wall (seghandler x-start x-end)
  (format t "clipping portal wall~%")
  (let* ((curr-wall (loop for i from x-start below x-end collect i))
         (intersection (intersection curr-wall (screen-range seghandler))))
    (if intersection
        (if (= (length intersection) (length curr-wall))
            (draw-portal-wall seghandler x-start (- x-end 1))
            (let* ((arr (sort intersection #'<))
                   (x (first arr)))
	      (loop for x1 in arr and x2 in (cdr arr) do
                (if (> (- x2 x1) 1)
                    (progn
		      (draw-portal-wall seghandler x x1)
		      (setf x x2))
                    ;; No else clause needed here
                    ))
	      (draw-portal-wall seghandler x (car (last arr))))))))

(defmethod classify-segment (seghandler segment x1 x2 rw-angle1)
  (format t "classifying segment~%")
  (setf (seg seghandler) segment
        (rw-angle1 seghandler) rw-angle1)
  (when (= x1 x2)
      (return-from classify-segment))
  (let ((bsector (wad-types::bsector segment))
        (fsector (wad-types::fsector segment)))
    
    ;; solid walls
    (if (null bsector)
        (progn
          (clip-solid-wall seghandler x1 x2)
          (return-from classify-segment))
        (progn
          (if (or (not (= (wad-types::ceilingheight fsector) (wad-types::ceilingheight bsector)))
                  (not (= (wad-types::floorheight fsector) (wad-types::floorheight bsector))))
              (progn
                (clip-portal-wall seghandler x1 x2)
                (return-from classify-segment))
              (progn
                (if (and (= (wad-types::ceilingflat fsector) (wad-types::ceilingflat bsector))
                         (= (wad-types::floorflat fsector) (wad-types::floorflat bsector))
                         (= (wad-types::lightlevel fsector) (wad-types::lightlevel bsector))
                         (string= (wad-types::middle (wad-types::frontside (wad-types::ldef segment))) "-"))
                    (return-from classify-segment)
                    (clip-portal-wall seghandler x1 x2))))))))



