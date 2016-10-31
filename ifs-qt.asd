;;;; ifs-qt.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:ifs-qt
  :description "Interactive IFS fracal viewer."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qt-libs
               #:qtools
               #:qtgui
               #:qtcore
               #:qtopengl
               #:cl-opengl
               #:cl-glu)
  :serial t
  :components ((:file "package")
               (:file "ifs-qt")))

