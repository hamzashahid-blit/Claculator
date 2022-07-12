(in-package :claculator)

(defparameter *text-style*
  (c:make-text-style :sans-serif :roman :large))

(defun make-button (label operator &key width height
                                        (max-width c:+fill+) min-width
                                        (max-height c:+fill+) min-height)
  (c:make-pane 'c:push-button
    :label label
    :activate-callback operator
    :text-style *text-style*
    :width width :height height
    :max-width  max-width  :min-width  min-width
    :max-height max-height :min-height min-height))


(c:define-application-frame claculator ()
  ((state :initform ""
          :accessor state)
   (screen-field :initform nil
                 :accessor screen-field))
  (:pointer-documentation nil)
  (:reinitialize-frames t)
  (:menu-bar nil)
  (:panes
    (screen :text-editor
            :value ""
            :text-style *text-style*) 
    (one   (make-button "1" (queue-object 1)))
    (two   (make-button "2" (queue-object 2)))
    (three (make-button "3" (queue-object 3)))
    (four  (make-button "4" (queue-object 4)))
    (five  (make-button "5" (queue-object 5)))
    (six   (make-button "6" (queue-object 6)))
    (seven (make-button "7" (queue-object 7)))
    (eight (make-button "8" (queue-object 8)))
    (nine  (make-button "9" (queue-object 9)))
    (zero  (make-button "0" (queue-object 0)))
    (add   (make-button "+" (queue-object '+)))
    (sub   (make-button "-" (queue-object '-)))
    (mult  (make-button "*" (queue-object '*)))
    (div   (make-button "/" (queue-object '/)))
    (eval  (make-button "="  #'calculate))
    (ac    (make-button "AC" #'init-ac))
    (ce    (make-button "CE" #'init-ce))
    (int :interactor))
  (:layouts
    (default (c:vertically ()         ;(:width 150 :max-width 500)
               (setf (screen-field c:*application-frame*) screen)
               ;; screen
               (c:horizontally ()     ;(:height 50)
                 ac ce)
               (c:tabling (:grid t)
                 (list one two three add)
                 (list four five six sub)
                 (list seven eight nine mult)
                 (list zero div eval int))))))

(defgeneric (setf app-state) (value))
(defmethod (setf app-state) (value)
  (setf (state c:*application-frame*) value))

(defun app-state ()
  (state c:*application-frame*))

(defun show ()
  (setf (c:gadget-value (screen-field c:*application-frame*))
        (app-state)))

(defun queue-object (object)
  (lambda (gadget)
    (declare (ignore gadget))
    (setf (state c:*application-frame*)
          (str:concat (app-state) (write-to-string object)))
    ;; (format t "Queued object: ~a ; State: ~a~%" object (app-state))
    (show)))

(defun calculate (gadget)
  (declare (ignore gadget))
  (setf (state c:*application-frame*)
        (write-to-string (eval (parse-maths (app-state)))))
  (show))

(defun init-ac (gadget)
  (declare (ignore gadget))
  (setf (state c:*application-frame*) "")
  (show))

;; TODO: Make a history feature
(defun init-ce (gadget)
  (declare (ignore gadget)))

(define-claculator-command (com-quit :name t) ()
  (init-ac t)
  (c:frame-exit c:*application-frame*))

(defun claculator ()
  (c:run-frame-top-level (c:make-application-frame 'claculator)))

(defun claculator-mt ()
  "Run the application in a separate thread. The mt stands for Multi-Threaded"
  (bt:make-thread
    (lambda () (c:run-frame-top-level (c:make-application-frame 'claculator)))))
