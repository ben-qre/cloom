;; classes.lisp

(defpackage :clock
  (:use :common-lisp :settings))

(in-package :clock)

(defclass clock ()
  ((engine      :accessor engine      :initarg :engine)
   (last-time   :accessor last-time   :initform (get-internal-real-time))
   (fps-history :accessor fps-history :initform '())))

(defpackage :bsp
  (:use :common-lisp :wad-types :angle :settings))

(in-package :bsp)

(defclass bsp ()
  ((engine          :accessor engine          :initarg :engine)
   (player          :accessor player          :initarg :player)
   (nodes           :accessor nodes           :initarg :nodes)
   (ssectors        :accessor ssectors        :initarg :ssectors)
   (segs            :accessor segs            :initarg :segs)
   (is-traverse     :accessor is-traverse     :initarg :is-traverse :initform t)
   (root-node-id    :accessor root-node-id)))

(defpackage :player
  (:use :cl :angle :settings))

(in-package :player)

(defclass player ()
  ((engine  :initarg :engine    :accessor engine)
   (x       :initarg :x         :accessor x)
   (y       :initarg :y         :accessor y)
   (rot     :initarg :rot-speed :accessor rot-speed)
   (angle   :initarg :angle     :accessor angle)
   (height                      :accessor height :initform PLAYER_HEIGHT)))

(defpackage :seghandler
  (:use :common-lisp :settings :angle))

(in-package :seghandler)

(defclass seghandler ()
  ((engine          :accessor engine          :initarg :engine)
   (map-data        :accessor map-data        :initarg :map-data)
   (player          :accessor player          :initarg :player)
   (seg             :accessor seg             :initform nil)
   (rw-angle1       :accessor rw-angle1       :initform nil)
   (screen-range    :accessor screen-range    :initform nil)
   (angles          :accessor angles          :initarg :angles :initform '())
   (upper-clip      :accessor upper-clip      :initform '())
   (lower-clip      :accessor lower-clip      :initform '())))


(defpackage :engine
  (:use :cl :map-data :bsp :player))

(in-package :engine)

(defclass CloomEngine ()
  ((engine-map-data :accessor engine-map-data :initarg :engine-map-data)
   (player          :accessor player          :initarg :player)
   (bsp             :accessor bsp             :initarg :bsp)
   (seghandler      :accessor seghandler      :initarg :seghandler)
   (render-engine   :accessor render-engine   :initarg :render-engine)
   (running         :accessor running         :initform t)
   (clock           :accessor clock           :initarg :clock)
   (time-delta      :accessor time-delta      :initarg :time-delta :initform 1/60) ;; 60 fps
   ))

(in-package :bsp)

(defmethod make-bsp (engine)
  (let ((bsp (make-instance 'bsp
                 :engine engine
                 :player (engine::player engine)
                 :nodes (map-data::nodes (engine::engine-map-data engine))
                 :ssectors (map-data::ssectors (engine::engine-map-data engine))
                 :segs (map-data::segs (engine::engine-map-data engine)))))
    (setf (root-node-id bsp) (- (length (nodes bsp)) 1))
    bsp))

(in-package :clock)

(defun make-clock (engine)
  (make-instance 'clock :engine engine))

(in-package :player)

(defun make-player (engine)
  (let ((start-info (nth 0 (map-data::things (engine::engine-map-data engine)))))
    (make-instance 'player
		   :engine engine
                   :x (wad-types::x start-info)
                   :y (wad-types::y start-info)
                   :rot-speed PLAYER_ROT_SPEED
                   :angle (angle::make-angle (wad-types::angle start-info)))))

(in-package :seghandler)

(defun make-angle-table ()
  (let ((table '()))
    (dotimes (i (+ SCREEN_WIDTH 1) table)
      (push (make-angle (* (atan (/ (- HALF_WIDTH i) SCREEN_DIST)) (/ 180 PI))) table))
    (nreverse table)))
      

(defun make-seghandler (engine)
  (let ((seghandler (make-instance 'seghandler
				   :engine engine
				   :map-data (engine::engine-map-data engine)
				   :player (engine::player engine)
				   :angles (make-angle-table))))
    seghandler))		   

(in-package :engine)

(defmethod make-engine (map-name)
  (make-instance 'CloomEngine
		 :engine-map-data (map-data::map-data-init map-name)))

(defmethod engine-init (engine)
  (setf (slot-value engine 'clock) (clock::make-clock engine))
  (setf (slot-value engine 'player) (player::make-player engine))
  (setf (slot-value engine 'bsp) (bsp::make-bsp engine))
  (setf (slot-value engine 'seghandler) (seghandler::make-seghandler engine))
  ;; (setf (slot-value engine 'render-engine) (render-engine:: ***funktionsname*** engine))
  engine)

(defmethod update (engine)
  (player::update (player engine))
  (seghandler::update (seghandler engine))
  (bsp::update (bsp engine))
  (setf (time-delta engine) (clock::tick (clock engine) 60)) ;; nach clock noch ändern
  )

(defmethod run (engine)
  ;; driver loop
  )
