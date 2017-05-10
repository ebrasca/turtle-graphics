;;;; turtle-graphics.asd
;;;; Copyright (C) 2017 Bruno Cichon <ebrasca.ebrasca@openmailbox.org>
;;;; This code is licensed under the GPLv3 license.

(asdf:defsystem #:turtle-graphics
  :description "This make computer graphics defined by turtle movements."
  :author "Bruno Cichon <ebrasca.ebrasca@openmailbox.org>"
  :license "GPLv3"
  :depends-on (#:iterate
               #:rtg-math)
  :serial t
  :components ((:file "package")
               (:file "turtle")))
