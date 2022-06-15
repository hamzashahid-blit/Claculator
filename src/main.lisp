(in-package :claculator)

(c:define-application-frame claculator ()
  ()
  (:pointer-documentation t)
  (:reinitialize-frames t)
  (:panes
    (app :application
         :width 300
         :height 500)
    (int :interactor
         :width 300
         :height 100))
  (:layouts
    (default (c:vertically ()
               app int))))

(defun run ()
  (bt:make-thread
    (lambda ()
      (c:run-frame-top-level (c:make-application-frame 'claculator)))))
