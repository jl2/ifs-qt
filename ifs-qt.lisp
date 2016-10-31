;;;; ifs-qt.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:ifs-qt)
(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 1) (compilation-speed 0) (debug 1)))

(defstruct transform
  (prob 1.0d0 :type double-float)
  (a 0.0d0 :type double-float)
  (b 0.0d0 :type double-float)
  (c 0.0d0 :type double-float)
  (d 0.0d0 :type double-float)
  (e 0.0d0 :type double-float)
  (f 0.0d0 :type double-float))

(declaim (inline xform))
(defun xform (pt xform)
  (declare (type pt cons)
           (type transform xform))
  (with-slots (a b c d e f) xform
    (let* ((x (car pt))
           (y (cdr pt))
           (nx (+ (* a x) (* b y) e))
           (ny (+ (* c x) (* d y) f)))
      (declare (type double-float a b c d e f x y nx ny))
      (setf (car pt) nx)
      (setf (cdr pt) ny)))
  pt)


(defstruct ifs-fractal
  (max-iterations 80000 :type fixnum)
  (max-radius 10.0d0 :type double-float)
  (transforms ))

(defun random-transform (transforms)
  (find-if (curry #'< (random 1.0d0)) transforms :key #'transform-prob))

(defun max-radius (transforms)
  (* 1.75 (sqrt 
           (loop
              for i below 50
              for xform = (random-transform transforms) then (random-transform transforms)
              for pt = '(1.0d0 . 1.0d0) then (xform pt xform)
              maximizing (+ (* (car pt) (car pt)) (* (cdr pt) (cdr pt)))))))

(defun create-ifs (max-iterations transforms)
  (make-ifs-fractal :max-iterations max-iterations
                    :max-radius (max-radius transforms)
                    :transforms transforms))


(define-widget ifs-animator (QGLWidget)
  ((ifs :initform (create-ifs 120000
                              (list 
                               (make-transform :prob 0.84d0 :a 0.85d0 :b 0.04d0 :c -0.04d0 :d 0.85d0 :e 0.00d0 :f 1.60d0)
                               (make-transform :prob 0.91d0 :a -0.15d0 :b 0.28d0 :c 0.26d0 :d 0.24d0 :e 0.00d0 :f 0.44d0)
                               (make-transform :prob 0.98d0 :a 0.20d0 :b -0.26d0 :c 0.23d0 :d 0.22d0 :e 0.00d0 :f 1.60d0)
                               (make-transform :prob 1.00d0 :a 0.00d0 :b 0.00d0 :c 0.00d0 :d 0.16d0 :e 0.00d0 :f 0.00d0)))
        :type ifs-fractal))
  (:documentation "Draw IFS fractals using OpenGL."))

(define-initializer (ifs-animator setup)
  (setf (q+:auto-fill-background ifs-animator) nil)
  (setf (q+:auto-buffer-swap ifs-animator) nil))

(define-override (ifs-animator initialize-G-L) ()
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test))

(define-override (ifs-animator resize-g-l) (width height))

(define-override (ifs-animator paint-g-l paint) ()
  "Handle paint events."
  (with-slots (max-iterations max-radius transforms) ifs
    (let* ((width (q+:width ifs-animator))
           (height (q+:height ifs-animator))
           (x-aspect-ratio (if (< height width)
                               (/ height width 1.0d0)
                               1.0d0))
           (y-aspect-ratio (if (< height width)
                               1.0d0
                               (/ width height 1.0d0))))

      (with-finalizing 
          ;; Create a painter object to draw on
          ((painter (q+:make-qpainter ifs-animator)))

        (q+:begin-native-painting painter)
        (gl:viewport 0 0 width height)
        (gl:matrix-mode :projection)
        (gl:load-identity)
        (gl:ortho (- max-radius) max-radius (- max-radius) max-radius -1.0 1.0)
        (gl:clear-color 0 0 0 1)

        (gl:enable :line-smooth :polygon-smooth
                   :point-smooth :blend
                   :depth-test :depth-clamp :alpha-test)
        (gl:blend-func :src-alpha :one-minus-src-alpha)

        (gl:matrix-mode :modelview)
        (gl:load-identity)

        (gl:clear :color-buffer :depth-buffer)
        (when transforms
          
          ;; Actual drawing goes here.  In this case, just a line.
          (gl:push-matrix)

          (gl:with-primitives :points
            (gl:color 0 1.0 0 0.85)
            (loop
               for i below max-iterations
               for xform = (random-transform transforms) then (random-transform transforms)
               for pt = '(1.0d0 .  1.0d0) then (xform pt xform)
               do
                 (gl:vertex (* x-aspect-ratio (car pt))
                            (* y-aspect-ratio (cdr pt))
                            0.0)))
          (gl:pop-matrix))
        (q+:swap-buffers ifs-animator)
        (q+:end-native-painting painter)))))

(define-widget ifs-widget (QWidget)
  ()
  (:documentation "A IFS animator and its controls."))

(define-subwidget (ifs-widget ifs-viewer) (make-instance 'ifs-animator)
  "The ifs-drawer itself.")


(define-subwidget (ifs-widget iterations-spin) (q+:make-qspinbox ifs-widget)
  "The spinbox for the number of iterations."
  (q+:set-maximum iterations-spin 10000000)
  (q+:set-minimum iterations-spin 1000)
  (q+:set-value iterations-spin (ifs-fractal-max-iterations (slot-value ifs-viewer 'ifs))))


(define-slot (ifs-widget iterations-changed) ((value int))
  "Handle changes to the iterations-spin box."
  (declare (connected iterations-spin (value-changed int)))
  (setf (ifs-fractal-max-iterations (slot-value ifs-viewer 'ifs)) (q+:value iterations-spin))
  (q+:repaint ifs-viewer))


(define-subwidget (ifs-widget transforms-edit) (q+:make-qtextedit ifs-widget)
  (setf (q+:maximum-height transforms-edit) 400)
  (setf (q+:text transforms-edit) (with-standard-io-syntax 
                                    (with-output-to-string (str)
                                      (pprint (slot-value (slot-value ifs-viewer 'ifs) 'transforms) str)))))

(define-slot (ifs-widget text-changed) ()
  (declare (connected transforms-edit (text-changed void)))

  (with-slots (max-radius transforms) (slot-value ifs-viewer 'ifs)
    (handler-case
        (progn 
          (setf transforms (read-from-string (q+:to-plain-text transforms-edit)))
          (setf max-radius (max-radius transforms)))
      (error () t)))
  (q+:repaint ifs-viewer))

(define-subwidget (ifs-widget control-layout) (q+:make-qvboxlayout ifs-widget)
  "Layout all of the control widgets in a vertical box layout."

  ;; Create horizontal layouts to hold the labels and spinboxes
  (let ((iteration-layout (q+:make-qhboxlayout))
        (transform-layout (q+:make-qhboxlayout)))
    
    ;; Populate the horizontal layouts and add them to the top level vertical layout

    (q+:add-widget iteration-layout (q+:make-qlabel "Iterations: " ifs-widget))
    (q+:add-widget iteration-layout iterations-spin)

    (q+:add-widget transform-layout (q+:make-qlabel "Transforms: " ifs-widget))
    (q+:add-widget transform-layout transforms-edit)

    (q+:add-layout control-layout iteration-layout)
    (q+:add-layout control-layout transform-layout)

    ;; Finally add the ifs viewer directly to the vertical layout
    (q+:add-widget control-layout ifs-viewer)))


(define-slot (ifs-widget open-config) ((file-name string))
  (declare (connected ifs-widget (open-config string)))
  (with-open-file (str file-name :direction :input)
    (with-standard-io-syntax 
      (let ((new-ifs (read str)))
        (with-slots (max-iterations max-radius transforms) new-ifs
          (q+:set-value iterations-spin max-iterations)
          (q+:set-value transforms-edit transforms))
        (setf (slot-value ifs-viewer 'ifs) new-ifs)))))

(define-slot (ifs-widget save-config) ((file-name string))
  (declare (connected ifs-widget (save-config string)))
  (with-open-file (str file-name :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (with-standard-io-syntax 
      (pprint (slot-value ifs-viewer 'ifs) str))))

(define-widget main-window (QMainWindow)
  ())

(define-override (main-window close-event) (ev)
  (q+:accept ev))


(define-menu (main-window File)
  (:item ("Open Config" (ctrl i))
         (open-config main-window))
  (:item ("Save Config" (ctrl s))
         (save-config main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate IFS fractals.")))

(define-subwidget (main-window ifs-viewer) (make-instance 'ifs-widget)
  "The central ifs-widget.")

(define-slot (main-window openc open-config) ()
  (let ((filename (q+:qfiledialog-get-open-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.home-location))
                                                     "*.txt")))
    (when (and filename (> (length filename) 0))
      (signal! ifs-viewer (open-config string) filename))))

(define-slot (main-window save save-config) ()
  (let ((filename (q+:qfiledialog-get-save-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.home-location))
                                                     "*.txt")))
    (when (and filename (> (length filename) 0))
      (signal! ifs-viewer (save-config string) filename))))

(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive IFS Explorer")
  (setf (q+:central-widget main-window) ifs-viewer))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))
