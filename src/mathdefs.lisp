(defpackage :math-defs
  (:use :common-lisp)
  (:export
    :cross-product-line
    :point-behind-segment
    :normalize
    :perp-2d
    :rotate-2d
    :distance-2d
    :to-radians
    :to-vector
    :intersection-2d))

(in-package :math-defs)

(defun cross-product-line (a b)
  (- (* (nth 0 a) (nth 1 b)) (* (nth 1 a) (nth 0 b))))

(defun point-behind-segment (point a b)
  (let ((cross (- (* (- (nth 0 b) (nth 0 a)) (- (nth 1 point) (nth 1 a)))
                   (* (- (nth 1 b) (nth 1 a)) (- (nth 0 point) (nth 0 a))))))
    (cond ((> cross 0) t)
          ((< cross 0) nil)
          ((= cross 0) nil))))

(defun normalize (a b)
  (if (or (/= a 0) (/= b 0))
      (let ((length (sqrt (+ (* a a) (* b b)))))
        (list (/ a length) (/ b length)))
      (list a b)))

(defun perp-2d (a b)
  (list (- b) a))

(defun rotate-2d (x y rads)
  (list (- (* x (cos rads)) (* y (sin rads)))
        (+ (* x (sin rads)) (* y (cos rads)))))

(defun distance-2d (x1 y1 x2 y2)
  (let ((x (- x1 x2))
        (y (- y1 y2)))
    (sqrt (+ (* x x) (* y y)))))

(defun to-radians (x y)
  (let ((v (normalize x y)))
    (atan (nth 1 v) (nth 0 v))))

(defun to-vector (rads)
  (list (cos rads) (sin rads)))

(defun intersection-2d (splitter-start splitter-end line-start line-end)
  (let ((s1 splitter-start)
        (e1 splitter-end)
        (s2 line-start)
        (e2 line-end))
    (let* ((a1 (- (nth 1 e1) (nth 1 s1)))
           (b1 (- (nth 0 s1) (nth 0 e1)))
           (c1 (+ (* a1 (nth 0 s1)) (* b1 (nth 1 s1))))
           (a2 (- (nth 1 e2) (nth 1 s2)))
           (b2 (- (nth 0 s2) (nth 0 e2)))
           (c2 (+ (* a2 (nth 0 s2)) (* b2 (nth 1 s2))))
           (delta (- (* a1 b2) (* a2 b1))))
      (if (/= delta 0)
          (list (/ (- (* b2 c1) (* b1 c2)) delta)
                (/ (- (* a1 c2) (* a2 c1)) delta))
          nil))))

(defun run-tests ()
  (format t "Running tests...~%")
  ; Did some test manually

  ; Test cross-product-line
  (let ((cross-product (cross-product-line '(2 3) '(4 5)))
        (expected -2))
    (assert (= cross-product expected))
    (format t "Test cross-product-line passed~%"))

  ; Test point-behind-segment
  (let ((point1 '(1 1))
        (point2 '(2 1))
        (point3 '(1 2))
        (expected1 nil)
        (expected2 nil)
        (expected3 t))
    (assert (equal (point-behind-segment point1 '(0 0) '(4 4)) expected1))
    (assert (equal (point-behind-segment point2 '(0 0) '(4 4)) expected2))
    (assert (equal (point-behind-segment point3 '(0 0) '(4 4)) expected3))
    (format t "Test point-behind-segment passed~%"))

  ; Test normalize
  (let* ((normalized (normalize 3 4))
         (expected '(0.6 0.8)))
    (assert (equal normalized expected))
    (format t "Test normalize passed~%"))

  ; Test perp-2d
  (let* ((perpendicular (perp-2d 3 4))
         (expected '(-4 3)))
    (assert (equal perpendicular expected))
    (format t "Test perp-2d passed~%"))

  ; Test distance-2d
  (let* ((distance (distance-2d 0 0 3 4))
         (expected 5))
    (assert (= distance expected))
    (format t "Test distance-2d passed~%"))

  ; Test intersection-2d
  (let* ((intersection (intersection-2d '(0 0) '(2 2) '(1 0) '(1 2)))
         (expected '(1 1)))
    (assert (equal intersection expected))
    (format t "Test intersection-2d passed~%"))

  (format t "All tests passed.~%"))



