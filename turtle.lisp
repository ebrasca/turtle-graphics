;;;; turtle.lisp
;;;; Copyright (C) 2017 Bruno Cichon <ebrasca.ebrasca@openmailbox.org>
;;;; This code is licensed under the GPLv3 license.

(in-package #:turtle-graphics)

;;; "turtle-graphics" goes here. Hacks and glory await!

(defparameter *i* -1)
(defparameter *vertices* nil)
(defparameter *index* nil)

(defclass turtle ()
  ((points :accessor points
           :initform nil)
   (translation :accessor tra
                :initform (v! 0.0 0.0 0.0 0.0))
   (rotation :accessor rot
             :initform (m4:identity))
   (pile :accessor pile
         :initform '())))

;;; Extrusion
(defun translation (turtle n)
  (setf (tra turtle)
        (v4:+ (v4:*S (m4:get-column (rot turtle) 1) n)
              (tra turtle))))

(defun extrusion (turtle n)
  (mapcar #'(lambda (vertex)
              (add-point
               (m4:get-column
                (m4:* (m4:translation (v4:+ (tra turtle) (v4:*S (m4:get-column (rot turtle) 1) n)))
                      (rot turtle)
                      (m4:translation (v4:- (aref *vertices* vertex) (tra turtle))))
                3)))
          (points turtle)))

(defun forward (turtle n)
  "Move turtle n units forward and draw"
  (let ((new-points (extrusion turtle n)))
    (dolist (item (triangulate (points turtle) new-points))
      (push item
            *index*))
    (setf (points turtle)
          new-points)
    (translation turtle n)))

(defun jump (turtle n)
  "Move turtle n units forward"
  (let ((new-points (extrusion turtle n)))
    (dolist (item new-points)
      (push item
            *index*))
    (setf (points turtle)
          new-points)
    (translation turtle n)))

;;; Rotation
(defun roll (turtle u)
  "Rotate in axis j by u angle"
  (let ((tmp (m4:get-column (m4:identity) 1)))
    (setf (rot turtle)
          (m4:* (m4:rotation-from-axis-angle (v! (aref tmp 0)
                                                 (aref tmp 1)
                                                 (aref tmp 2))
                                             u)
                (rot turtle)))))

(defun pitch (turtle v)
  "Rotate in axis k by v angle"
  (let ((tmp (m4:get-column (m4:identity) 2)))
    (setf (rot turtle)
          (m4:* (m4:rotation-from-axis-angle (v! (aref tmp 0)
                                                 (aref tmp 1)
                                                 (aref tmp 2))
                                             v)
                (rot turtle)))))

(defun yaw (turtle w)
  "Rotate in axis i by w angle"
  (let ((tmp (m4:get-column (m4:identity) 0)))
    (setf (rot turtle)
          (m4:* (m4:rotation-from-axis-angle (v! (aref tmp 0)
                                                 (aref tmp 1)
                                                 (aref tmp 2))
                                             w)
                (rot turtle)))))

;;; Other
(defun push-turtle (turtle)
  "Remember the current state of turtle"
  (push (list (copy-list (points turtle))
              (copy-seq (rot turtle))
              (copy-seq (tra turtle)))
        (pile turtle)))

(defun pop-turtle (turtle)
  "Restore the last remembered state of turtle and remove it from the list of remembered states"
  (let ((pile (pop (pile turtle))))
    (setf (points turtle) (first pile)
          (rot turtle) (second pile)
          (tra turtle) (third pile))))

;;; Geometry
(defun add-point (point)
  (incf *i*)
  
  (when (= *i* (car (array-dimensions *vertices*)))
    (adjust-array *vertices* (list (* 2 (car (array-dimensions *vertices*))))))
  
  (setf (aref *vertices* *i*) point)
  *i*)

(defun circle (turtle n r)
  (iter (with angle := (/ (* 2 3.1415927) n))
        (for i :from 0 :to n)
        (for res := (cons (add-point (m4:*v (m4:rotation-y (* i angle))
                                            (v! r 0.0 0.0)))
                          res))
        (finally (setf (points turtle)
                       (cons 0 res)))))

(defun triangulate (index0 index1)
  "2__3
   | /|
   |/ |
   0__1"
  (iter (with p0 = (first index0))
        (with p2 = (first index1))
        (for p1 in (cdr index0))
        (for p3 in (cdr index1))
        (nconcing (list p0 p1 p3 p3 p2 p0))
        (setf p0 p1
              p2 p3)))

(defun make-geometry (l-system)
  (let ((*i* -1)
        (*vertices* (make-array '(10) :adjustable t))
        (*index* '())
        (turtle (make-instance 'turtle)))
    (iter (for (symbol . parameter) :in l-system)
          (if parameter
              (apply symbol turtle parameter)
              (funcall symbol turtle)))
    (values *vertices*
            *index*)))
