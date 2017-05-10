;;;; turtle.lisp
;;;; Copyright (C) 2017 Bruno Cichon <ebrasca.ebrasca@openmailbox.org>
;;;; This code is licensed under the GPLv3 license.

(in-package #:turtle-graphics)

;;; "turtle-graphics" goes here. Hacks and glory await!

(defparameter *i* -1)
(defparameter *vertices* nil)
(defparameter *index* nil)

(defclass turtle ()
  ((base :accessor base
         :initform nil)
   (points :accessor points
           :initform nil)
   (translation :accessor tra
                :initform (v! 0.0 0.0 0.0 0.0))
   (rotation :accessor rot
             :initform (v! 0.0 0.0 0.0))
   (pile :accessor pile
         :initform '())))

;;; Extrusion
(defun forward (turtle n)
  "Move turtle n units forward"
  (let* ((translation (v4:+ (tra turtle)
                            (m4:get-column
                             (m4:* (m4:rotation-from-euler (rot turtle))
                                   (m4:translation (v! 0.0 n 0.0)))
                             3)))
         (new-points (mapcar #'(lambda (vertex)
                                 (add-point
                                  (let ((tra (m4:get-column
                                              (m4:* (m4:translation translation)
                                                    (m4:rotation-from-euler (rot turtle))
                                                    (m4:translation (aref *vertices* vertex)))
                                              3)))
                                    (v! (aref tra 0)
                                        (aref tra 1)
                                        (aref tra 2)))))
                             (base turtle))))
    (dolist (item (triangulate (points turtle) new-points))
      (push item
            *index*))
    (setf (points turtle)
          new-points

          (tra turtle)
          translation)))

;;; Rotation
(defun roll (turtle u)
  (setf (rot turtle)
        (v:+ (rot turtle)
             (v! u 0.0 0.0))))

(defun pitch (turtle v)
  (setf (rot turtle)
        (v:+ (rot turtle)
             (v! 0.0 v 0.0))))

(defun yaw (turtle w)
  (setf (rot turtle)
        (v:+ (rot turtle)
             (v! 0.0 0.0 w))))

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

(defun circle (turtle n)
  (iter (with angle := (/ (* 2 3.1415927) n))
        (for i :from 0 :to n)
        (for res := (cons (add-point (m3:*v (m3:rotation-y (* i angle))
                                            (v! 1.0 0.0 0.0)))
                          res))
        (finally (setf (points turtle)
                       (cons 0 res)
                       (base turtle)
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
              (funcall symbol turtle (car parameter))
              (funcall symbol turtle)))
    (values *vertices*
            *index*)))
