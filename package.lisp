;;;; package.lisp
;;;; Copyright (C) 2017 Bruno Cichon <ebrasca.ebrasca@openmailbox.org>
;;;; This code is licensed under the GPLv3 license.

(defpackage #:turtle-graphics
  (:use #:cl #:iterate :rtg-math)
  (:export #:turtle
           ;;extrude
           #:forward
           #:jump
           ;;rotation
           #:roll
           #:pitch
           #:yaw
           ;;other
           #:make-geometry
           #:circle
           #:push-turtle
           #:pop-turtle))
