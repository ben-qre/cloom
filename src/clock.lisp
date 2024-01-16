;; clock.lisp

(in-package :clock)

(defmethod tick (clock &optional (framerate nil))
  (let ((current-time (get-internal-real-time))
        (ltime (last-time clock))
	delay-ms target-time)
    (setf (last-time clock) current-time)
    ;; fps history updating
    (let ((elapsed-ms (* (float (/ (- current-time ltime) internal-time-units-per-second)) 1000)))
      (push elapsed-ms (fps-history clock))
      (when (> (length (fps-history clock)) 10)
	(setf (fps-history clock) (subseq (fps-history clock) 0 10)))
      ;; framerate limiting
      (when framerate
        (setf target-time (+ ltime (/ internal-time-units-per-second framerate)))
        (when (< current-time target-time)
          (setf delay-ms (- target-time current-time))
          (sleep (/ delay-ms internal-time-units-per-second))))
      elapsed-ms)))

(defun average (list)
  (/ (reduce '+ list) (length list)))

(defmethod get-fps (clock)
  (/ 1000 (average (fps-history clock))))

